#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use std::{collections::hash_map::Entry, sync::Arc};
use swc_atoms::JsWord;
use swc_common::{util::move_map::MoveMap, Fold, FoldWith, Spanned, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ts_checker::{ty, ty::Type, util::TypeEq, ModuleTypeInfo};

pub fn generate_dts(module: Module, info: ModuleTypeInfo) -> Module {
    module.fold_with(&mut TypeResolver {
        info,
        current_class: None,
        top_level: true,
    })
}

#[derive(Debug)]
struct TypeResolver {
    info: ModuleTypeInfo,
    current_class: Option<ty::Class>,
    top_level: bool,
}

impl TypeResolver {
    fn take<F>(&mut self, sym: &JsWord, mut pred: F) -> Option<Arc<Type>>
    where
        F: FnMut(&Type) -> bool,
    {
        if let Some(types) = self.info.types.get_mut(sym) {
            for ty in &*types {
                debug_assert!(ty.is_arc(), "All exported types must be freezed");
            }

            let pos = types.iter().position(|ty| pred(ty.normalize()));
            if let Some(pos) = pos {
                return Some(match types.remove(pos) {
                    Type::Arc(ty) => ty,
                    _ => unreachable!(),
                });
            }
        }

        None
    }

    fn get_mapped<F, T>(&self, sym: &JsWord, mut pred: F) -> Option<T>
    where
        F: FnMut(&Type) -> Option<T>,
    {
        if let Some(types) = self.info.types.get(sym) {
            for ty in &*types {
                debug_assert!(ty.is_arc(), "All exported types must be freezed");
            }

            types.iter().filter_map(|ty| pred(ty.normalize())).next()
        } else {
            None
        }
    }
}

impl Fold<BlockStmt> for TypeResolver {
    fn fold(&mut self, mut node: BlockStmt) -> BlockStmt {
        let old = self.top_level;
        self.top_level = false;
        node = node.fold_children(self);
        self.top_level = old;

        node
    }
}

impl Fold<Function> for TypeResolver {
    fn fold(&mut self, mut node: Function) -> Function {
        let old = self.top_level;
        self.top_level = false;
        node = node.fold_children(self);
        self.top_level = old;

        node
    }
}

impl Fold<VarDecl> for TypeResolver {
    fn fold(&mut self, node: VarDecl) -> VarDecl {
        let node = node.fold_children(self);

        VarDecl {
            declare: true,
            ..node
        }
    }
}

impl Fold<VarDeclarator> for TypeResolver {
    fn fold(&mut self, mut node: VarDeclarator) -> VarDeclarator {
        match node.init {
            Some(box Expr::Lit(Lit::Null(..)))
            | Some(box Expr::Lit(Lit::Str(..)))
            | Some(box Expr::Lit(Lit::JSXText(..))) => {
                node.init = None;
            }
            Some(box Expr::Lit(..)) => {}
            _ => {
                node.init = None;
            }
        };

        if node.init.is_some() {
            return node;
        }

        match node.name {
            Pat::Ident(ref mut node) => {
                if node.type_ann.is_none() {
                    if let Some(ty) = self.info.vars.remove(&node.sym).map(From::from) {
                        node.type_ann = Some(TsTypeAnn {
                            span: DUMMY_SP,
                            type_ann: box ty,
                        });
                    }
                }
            }
            _ => {}
        }

        if node.init.is_none() {
            node.name = node.name.fold_with(self);
        }

        node
    }
}

impl Fold<FnDecl> for TypeResolver {
    fn fold(&mut self, node: FnDecl) -> FnDecl {
        if node.function.return_type.is_some() {
            return node;
        }

        let node: FnDecl = node.fold_children(self);

        let return_type = self.get_mapped(&node.ident.sym, |ty| match ty {
            Type::Function(ty::Function { ref ret_ty, .. }) => {
                Some(TsTypeAnn::from((**ret_ty).clone()))
            }
            _ => None,
        });

        FnDecl {
            declare: true,
            function: Function {
                return_type,
                ..node.function
            },
            ..node
        }
    }
}

impl Fold<TsModuleDecl> for TypeResolver {
    fn fold(&mut self, node: TsModuleDecl) -> TsModuleDecl {
        TsModuleDecl {
            declare: true,
            ..node.fold_children(self)
        }
    }
}

impl Fold<TsEnumDecl> for TypeResolver {
    fn fold(&mut self, node: TsEnumDecl) -> TsEnumDecl {
        let members = self.get_mapped(&node.id.sym, |ty| match ty {
            Type::Enum(e) => Some(
                e.members
                    .iter()
                    .map(|member| TsEnumMember {
                        span: member.span,
                        id: member.id.clone(),
                        init: Some(box Expr::Lit(match member.val.clone() {
                            TsLit::Number(v) => Lit::Num(v),
                            TsLit::Str(v) => Lit::Str(v),
                            TsLit::Bool(..) => {
                                unreachable!("enum member with bool value is invalid")
                            }
                        })),
                    })
                    .collect(),
            ),
            _ => None,
        });

        TsEnumDecl {
            declare: true,
            members: members.unwrap_or(node.members),
            ..node
        }
    }
}

impl Fold<TsTypeAliasDecl> for TypeResolver {
    fn fold(&mut self, node: TsTypeAliasDecl) -> TsTypeAliasDecl {
        TsTypeAliasDecl {
            declare: true,
            ..node.fold_children(self)
        }
    }
}

impl Fold<Option<BlockStmt>> for TypeResolver {
    #[inline]
    fn fold(&mut self, _: Option<BlockStmt>) -> Option<BlockStmt> {
        None
    }
}

impl Fold<ClassDecl> for TypeResolver {
    fn fold(&mut self, mut node: ClassDecl) -> ClassDecl {
        node.declare = true;

        let old = self.current_class.take();

        if let Some(class) = self.get_mapped(&node.ident.sym, |ty| match ty {
            Type::Class(class) => Some(class.clone()),
            _ => None,
        }) {
            self.current_class = Some(class);
        }

        node = node.fold_children(self);

        self.current_class = old;

        node
    }
}

impl Fold<ClassProp> for TypeResolver {
    fn fold(&mut self, mut node: ClassProp) -> ClassProp {
        node.value = None;

        if node.accessibility == Some(Accessibility::Private) {
            node.type_ann = None;
        }

        if node.type_ann.is_some() {
            return node;
        }

        if let Some(cls) = &mut self.current_class {
            if let Some(type_ann) = cls.body.iter().find_map(|v| match v {
                ty::ClassMember::Property(p) => {
                    //
                    if node.key.type_eq(&p.key) {
                        //
                        return p
                            .value
                            .clone()
                            .map(|ty| ty.generalize_lit().into_owned().into());
                    }

                    None
                }
                _ => None,
            }) {
                node.type_ann = Some(type_ann)
            }
        }

        node.fold_children(self)
    }
}

impl Fold<ClassMethod> for TypeResolver {
    fn fold(&mut self, mut node: ClassMethod) -> ClassMethod {
        node = node.fold_children(self);

        if node.function.return_type.is_some() {
            return node;
        }

        if node.kind != MethodKind::Method {
            return node;
        }

        if let Some(cls) = &mut self.current_class {
            if let Some(return_type) = cls.body.iter().find_map(|v| match v {
                ty::ClassMember::Method(m) => {
                    //
                    if node.key.type_eq(&m.key) {
                        //
                        return Some(TsTypeAnn::from(*m.ret_ty.clone()));
                    }

                    None
                }
                _ => None,
            }) {
                node.function.return_type = Some(return_type);
            }
        }

        node
    }
}

impl Fold<Vec<ClassMember>> for TypeResolver {
    fn fold(&mut self, mut members: Vec<ClassMember>) -> Vec<ClassMember> {
        members = members.fold_children(self);

        let mut props = Vec::with_capacity(members.len() + 6);
        let mut buf = Vec::with_capacity(members.len());
        //

        for mut m in members {
            match m {
                ClassMember::Constructor(ref mut c) => {
                    for p in c.params.iter_mut() {
                        match p {
                            PatOrTsParamProp::TsParamProp(ref mut p) => {
                                if p.accessibility.is_some() || p.readonly {
                                    props.push(ClassMember::ClassProp(ClassProp {
                                        span: Default::default(),
                                        key: box match &p.param {
                                            TsParamPropParam::Ident(p) => Expr::Ident(p.clone()),
                                            TsParamPropParam::Assign(p) => match &p.left {
                                                //
                                                box Pat::Ident(i) => Expr::Ident(i.clone()),
                                                _ => unreachable!(
                                                    "binding pattern in property initializer"
                                                ),
                                            },
                                        },
                                        value: None,
                                        type_ann: None,
                                        is_static: false,
                                        decorators: vec![],
                                        computed: false,
                                        accessibility: p.accessibility,
                                        is_abstract: false,
                                        is_optional: false,
                                        readonly: p.readonly,
                                        definite: false,
                                    }));
                                }

                                p.accessibility = None;
                                p.readonly = false;
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }

            buf.push(m);
        }

        props.extend(buf);

        props
    }
}

impl Fold<Vec<Stmt>> for TypeResolver {
    fn fold(&mut self, stmts: Vec<Stmt>) -> Vec<Stmt> {
        if !self.top_level {
            return vec![];
        }

        stmts.fold_children(self)
    }
}

impl Fold<Vec<ModuleItem>> for TypeResolver {
    fn fold(&mut self, items: Vec<ModuleItem>) -> Vec<ModuleItem> {
        items.move_flat_map(|item| match item {
            ModuleItem::ModuleDecl(_) | ModuleItem::Stmt(Stmt::Decl(..)) => {
                Some(item.fold_with(self))
            }

            _ => None,
        })
    }
}
