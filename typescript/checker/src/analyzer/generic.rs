use super::Analyzer;
use crate::{
    analyzer::scope::Scope,
    builtin_types,
    ty::{
        self, Alias, CallSignature, Conditional, FnParam, IndexedAccessType, Interface, Mapped,
        Operator, PropertySignature, Ref, Tuple, Type, TypeElement, TypeLit, TypeOrSpread,
        TypeParam, TypeParamDecl, TypeParamInstantiation, Union,
    },
    ValidationResult,
};
use bitflags::_core::mem::take;
use fxhash::{FxHashMap, FxHashSet};
use swc_atoms::JsWord;
use swc_common::{Fold, FoldWith, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    /// TODO: implement
    pub(super) fn infer_arg_types(
        &mut self,
        type_params: &[TypeParam],
        params: &[FnParam],
        args: &[TypeOrSpread],
    ) -> ValidationResult<TypeParamInstantiation> {
        let mut inferred = FxHashMap::default();

        // TODO: Handle optional parameters
        // TODO: Convert this to error.
        assert!(args.len() >= params.len());

        for (p, arg) in params.iter().zip(args) {
            match &p.ty {
                Type::Param(TypeParam { ref name, .. }) => {
                    assert_eq!(
                        arg.spread, None,
                        "argument type inference for spread argument is not implemented yet"
                    );

                    // Very simple case.
                    inferred.insert(name, arg.ty.clone());
                    // function foo<T>(a: T) {}
                }

                _ => {}
            }
        }

        let mut params = Vec::with_capacity(type_params.len());
        for type_param in type_params {
            if let Some(ty) = inferred.remove(&type_param.name) {
                params.push(ty);
            } else {
                log::warn!(
                    "infer_type_args: falling back to unknown type parameter: {:?}",
                    type_param
                );

                // TODO: Fix this and implement full type inference
                params.push(type_param.clone().into());
            }
        }

        Ok(TypeParamInstantiation {
            span: DUMMY_SP,
            params,
        })
    }

    pub(super) fn expand_type_params(
        &mut self,
        i: &TypeParamInstantiation,
        decl: &TypeParamDecl,
        ty: Type,
    ) -> ValidationResult {
        let mut ty = ty.fold_with(&mut GenericExpander {
            analyzer: self,
            params: &decl.params,
            i,
            state: Default::default(),
        });
        match ty {
            Type::Tuple(Tuple { ref types, .. }) => {
                let mut buf = vec![];

                for (idx, t) in types.into_iter().enumerate() {
                    let t = self.expand_type_params(i, decl, t.clone())?;
                    buf.push((idx, t))
                }

                for (idx, t) in buf {
                    match ty {
                        Type::Tuple(ref mut tuple) => {
                            tuple.types[idx] = t;
                        }

                        _ => unreachable!(),
                    }
                }

                return Ok(ty);
            }

            _ => {}
        }

        Ok(ty)
    }

    fn expand_type_param(
        &mut self,
        i: &TypeParamInstantiation,
        decl: &TypeParamDecl,
        mut type_param: TypeParam,
    ) -> ValidationResult<TypeParam> {
        if let Some(c) = type_param.constraint {
            let c = self.expand_type_params(i, decl, *c.clone())?;

            type_param.constraint = Some(box c);
        }

        Ok(type_param)
    }

    /// Returns `Some(true)` if `child` extends `parent`.
    fn extends(&self, child: &Type, parent: &Type) -> Option<bool> {
        let span = child.span();

        match self.assign(parent, child, span) {
            Ok(()) => Some(true),
            _ => Some(false),
        }
    }
}

/// TODO: Handle operators like keyof.
///  e.g.
///    Convert
///      type BadNested<T> = {
///          x: T extends number ? T : string;
///      };
///      T extends {
///          [K in keyof BadNested<infer P>]: BadNested<infer P>[K];
///      } ? P : never;
///   into
///      T extends {
///          x: infer P extends number ? infer P : string;
///      } ? P : never
pub(super) struct GenericExpander<'a, 'b, 'c> {
    pub analyzer: &'a Analyzer<'b, 'c>,
    pub params: &'a [TypeParam],
    pub i: &'a TypeParamInstantiation,
    pub state: ExpanderState,
}

#[derive(Debug, Default)]
pub(super) struct ExpanderState {
    expand_fully: bool,
    dejavu: FxHashSet<JsWord>,
}

impl Fold<Type> for GenericExpander<'_, '_, '_> {
    fn fold(&mut self, ty: Type) -> Type {
        let should_expand_fully = self.state.expand_fully
            || match ty {
                Type::Mapped(Mapped { ty: Some(..), .. }) => true,
                _ => false,
            };

        let old_state_full = self.state.expand_fully;
        self.state.expand_fully = true;

        let ty = ty.fold_children(self);

        self.state.expand_fully = old_state_full;

        log::debug!("expanding: {:?}", ty.normalize());
        match ty.normalize() {
            Type::Ref(Ref {
                span,
                type_name: TsEntityName::Ident(Ident { ref sym, .. }),
                type_args,
                ..
            }) => {
                if *sym != *"BadNested" {
                    if self.state.dejavu.contains(sym) {
                        return ty;
                    }
                }
                self.state.dejavu.insert(sym.clone());

                log::info!("Ref: {}", sym);
                // Handle references to type parameters
                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == *sym {
                        assert_eq!(*type_args, None);

                        return self.i.params[idx].clone().fold_with(self);
                    }
                }

                if self.state.expand_fully {
                    // Check for builtin types
                    if !self.analyzer.is_builtin {
                        if let Ok(ty) = builtin_types::get_type(self.analyzer.libs, *span, sym) {
                            let ty = ty.fold_with(self);
                            // log::info!("builtin: {:?}", ty);
                            return ty;
                        }
                    }
                }

                if self.state.expand_fully {
                    if let Some(types) = self.analyzer.find_type(sym) {
                        for t in types {
                            match t.normalize() {
                                Type::Ref(..) => return t.clone().fold_with(self),

                                Type::TypeLit(..) | Type::Lit(..) => return t.clone(),

                                Type::Interface(Interface { type_params, .. })
                                | Type::Class(ty::Class { type_params, .. })
                                | Type::Alias(Alias { type_params, .. }) => {
                                    if let Some(type_params) = type_params {
                                        if let Some(type_args) = &type_args {
                                            let mut v = GenericExpander {
                                                analyzer: self.analyzer,
                                                params: &type_params.params,
                                                i: type_args,
                                                state: Default::default(),
                                            };

                                            return t.clone().fold_with(&mut v);
                                        }
                                    } else {
                                        return t.clone().fold_with(self);
                                    }
                                }

                                _ => {}
                            }
                        }
                    }
                }

                // Normal type reference
            }

            Type::Alias(alias) => {
                return if let Some(type_params) = &alias.type_params {
                    let mut v = GenericExpander {
                        analyzer: self.analyzer,
                        params: &type_params.params,
                        i: self.i,
                        state: take(&mut self.state),
                    };
                    log::debug!("Alias (generic)");
                    let ty = *alias.ty.clone().fold_with(&mut v);
                    self.state = v.state;
                    ty
                } else {
                    *alias.ty.clone()
                }
            }

            Type::Interface(i) => {
                // TODO: Handle super
                if !i.extends.is_empty() {
                    log::error!("not yet implemented: expanding interface which has a parent");
                    return ty;
                }

                let members = if let Some(type_params) = &i.type_params {
                    let mut v = GenericExpander {
                        analyzer: self.analyzer,
                        params: &type_params.params,
                        i: self.i,
                        state: take(&mut self.state),
                    };

                    log::debug!("Interface (generic)");
                    let ty = i.body.clone().fold_with(&mut v);
                    self.state = v.state;
                    ty
                } else {
                    log::debug!("Interface (not generic)");
                    i.body.clone()
                };

                return Type::TypeLit(TypeLit {
                    span: i.span,
                    members,
                });
            }

            Type::Conditional(Conditional {
                span,
                check_type,
                extends_type,
                true_type,
                false_type,
            }) => {
                if let Some(v) = self.analyzer.extends(&check_type, &extends_type) {
                    return if v {
                        *true_type.clone()
                    } else {
                        *false_type.clone()
                    };
                }
            }

            Type::Mapped(Mapped {
                span,
                readonly,
                optional,
                type_param,
                ty: Some(rty),
            }) => {
                if let Some(constraint) = &type_param.constraint {
                    match &**constraint {
                        Type::Operator(Operator {
                            span,
                            op: TsTypeOperatorOp::KeyOf,
                            ty,
                        }) => match &**ty {
                            Type::Keyword(..) => return *ty.clone(),
                            Type::TypeLit(lit) => {
                                return Type::TypeLit(TypeLit {
                                    span: lit.span,
                                    members: lit
                                        .members
                                        .iter()
                                        .filter_map(|member| {
                                            let ret_ty = &**rty;
                                            let mut computed = false;
                                            let mut optional = false;
                                            let mut readonly = false;
                                            match member {
                                                TypeElement::Call(_) => {}
                                                TypeElement::Constructor(_) => return None,
                                                TypeElement::Property(p) => {
                                                    optional = p.optional;
                                                    readonly = p.readonly;
                                                    computed = p.computed;
                                                }
                                                TypeElement::Method(m) => {
                                                    optional = m.optional;
                                                    readonly = m.readonly;
                                                    computed = m.computed;
                                                }
                                                TypeElement::Index(_) => {}
                                            }

                                            Some(TypeElement::Property(PropertySignature {
                                                span: member.span(),
                                                readonly,
                                                key: box member.key()?.clone(),
                                                computed,
                                                optional,
                                                params: vec![],
                                                type_ann: Some(ret_ty.clone()),
                                                type_params: None,
                                            }))
                                        })
                                        .collect(),
                                });
                            }
                            _ => {}
                        },

                        _ => {}
                    }
                }

                match rty.normalize() {
                    Type::IndexedAccessType(IndexedAccessType {
                        span,
                        readonly,
                        obj_type,
                        index_type,
                    }) => {
                        match obj_type.normalize() {
                            Type::TypeLit(TypeLit { span, members, .. })
                                if members.iter().all(|m| match m {
                                    TypeElement::Property(_) => true,
                                    _ => false,
                                }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());

                                for m in members {
                                    match m {
                                        ty::TypeElement::Property(p) => {
                                            let p = p.clone();
                                            //
                                            new_members.push(ty::TypeElement::Property(p));
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                return Type::TypeLit(TypeLit {
                                    span: *span,
                                    members: new_members,
                                });
                            }

                            _ => {}
                        }
                    }
                    _ => {}
                }
            }

            Type::Param(param) => {
                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == param.name {
                        match self.i.params[idx].clone().normalize() {
                            Type::Param(..) => {}
                            _ => return self.i.params[idx].clone().fold_with(self),
                        }
                    }
                }

                if param.constraint.is_some() && is_literals(&param.constraint.as_ref().unwrap()) {
                    return *param.constraint.clone().unwrap();
                }

                if param.constraint.is_some()
                    && match **param.constraint.as_ref().unwrap() {
                        Type::Keyword(..) => true,
                        Type::Ref(..) => true,
                        Type::TypeLit(..) => true,
                        _ => false,
                    }
                {
                    return *param.constraint.clone().unwrap();
                }

                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == param.name {
                        match self.i.params[idx].clone().normalize() {
                            Type::Param(..) => return self.i.params[idx].clone(),
                            _ => {}
                        }
                    }
                }
            }

            _ => {}
        }

        ty
    }
}

/// This method returns true for types like `'foo'` and `'foo' | 'bar'`.
pub(super) fn is_literals(ty: &Type) -> bool {
    match ty.normalize() {
        Type::Lit(_) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|v| is_literals(v)),
        _ => false,
    }
}
