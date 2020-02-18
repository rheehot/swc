#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use std::{collections::hash_map::Entry, sync::Arc};
use swc_atoms::JsWord;
use swc_common::{Fold, FoldWith};
use swc_ecma_ast::*;
use swc_ts_checker::{ty, ty::Type, ModuleTypeInfo};

pub fn generate_dts(module: Module, info: ModuleTypeInfo) -> Module {
    println!("Info: {:?}", info);
    module.fold_with(&mut TypeResolver { info })
}

#[derive(Debug)]
struct TypeResolver {
    info: ModuleTypeInfo,
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

    fn take_mapped<F, T>(&mut self, sym: &JsWord, mut pred: F) -> Option<T>
    where
        F: FnMut(&Type) -> Option<T>,
    {
        if let Some(types) = self.info.types.get_mut(sym) {
            for ty in &*types {
                debug_assert!(ty.is_arc(), "All exported types must be freezed");
            }

            types.iter().filter_map(|ty| pred(ty.normalize())).next()
        } else {
            None
        }
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
            Some(box Expr::Lit(Lit::Null(..))) => {
                node.init = None;
            }
            Some(box Expr::Lit(..)) => {}
            _ => {
                node.init = None;
            }
        };

        match node.name {
            Pat::Ident(ref mut node) => {
                if let Some(ty) = self.info.vars.remove(&node.sym).map(From::from) {
                    node.type_ann = Some(TsTypeAnn {
                        span: Default::default(),
                        type_ann: box ty,
                    });
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

impl Fold<ClassDecl> for TypeResolver {
    fn fold(&mut self, node: ClassDecl) -> ClassDecl {
        ClassDecl {
            declare: true,
            ..node.fold_children(self)
        }
    }
}

impl Fold<FnDecl> for TypeResolver {
    fn fold(&mut self, node: FnDecl) -> FnDecl {
        if node.function.return_type.is_some() {
            return node;
        }

        let node: FnDecl = node.fold_children(self);

        let return_type = self.take_mapped(&node.ident.sym, |ty| match ty {
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
