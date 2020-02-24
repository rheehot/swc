use super::Analyzer;
use crate::{
    ty::{
        self, Conditional, FnParam, Mapped, Ref, Tuple, Type, TypeOrSpread, TypeParam,
        TypeParamDecl, TypeParamInstantiation, Union,
    },
    ValidationResult,
};
use fxhash::FxHashMap;
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
                log::warn!("infer_type_args: falling back to unknown type parameter");
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
        mut ty: Type,
    ) -> ValidationResult {
        match ty {
            Type::TypeLit(..) | Type::Keyword(..) | Type::Lit(..) => return Ok(ty),

            Type::Ref(Ref {
                type_name: TsEntityName::Ident(Ident { ref sym, .. }),
                ..
            }) => {
                // Handle references to type parameters
                for (idx, p) in decl.params.iter().enumerate() {
                    if p.name == *sym {
                        return Ok(Type::from(i.params[idx].clone()));
                    }
                }

                // Normal type reference
                return Ok(ty);
            }

            Type::Operator(mut op) => {
                let expanded = self.expand_type_params(i, decl, *op.ty)?;

                op.ty = box expanded;

                return Ok(Type::Operator(op));
            }

            Type::Conditional(Conditional {
                check_type,
                extends_type,
                true_type,
                false_type,
                ..
            }) => {
                let check_type = self.expand_type_params(i, decl, *check_type)?;
                if let Some(v) = self.extends(&check_type, &extends_type) {
                    return Ok(if v {
                        *true_type.clone()
                    } else {
                        *false_type.clone()
                    });
                }

                //
                unimplemented!(
                    "expanding conditional type.\nParams: {:#?}\nTypeParam decls: {:#?}\nCheck \
                     Type: {:#?}\nextends_type: {:#?}\nTrue type: {:#?}\nFalse type: {:#?}",
                    i,
                    decl,
                    check_type,
                    extends_type,
                    true_type,
                    false_type
                )
            }

            Type::Mapped(t) => {
                let type_param = self.expand_type_param(&i, decl, t.type_param)?;

                // TODO:
                //
                //     type T50<T> = { [P in keyof T]: number };
                //     type T51 = T50<any>;  // { [x: string]: number }
                //     type T52 = T50<unknown>;  // {}

                return Ok(Type::Mapped(Mapped { type_param, ..t }));
            }

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

            Type::Function(ty::Function {
                span,
                ref params,
                ref type_params,
                ref ret_ty,
                ..
            }) => {
                // assert_eq!(type_params.as_ref(), Some(decl));
                let mut v = GenericExpander {
                    params: &decl.params,
                    i,
                };
                let ret_ty = ret_ty.clone().fold_with(&mut v);
                let params = params.clone().fold_with(&mut v);
                let type_params = type_params.clone().fold_with(&mut v);
                return Ok(Type::Function(ty::Function {
                    span,
                    params,
                    type_params,
                    ret_ty,
                }));
            }

            _ => {}
        }

        unimplemented!(
            "expand_type_params({:#?})\nDecl: {:#?}\nInstantiation: {:#?}",
            ty,
            decl,
            i
        )
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
    fn extends(&mut self, child: &Type, parent: &Type) -> Option<bool> {
        let span = child.span();

        match self.assign(parent, child, span) {
            Ok(()) => Some(true),
            _ => Some(false),
        }
    }
}

pub(super) struct GenericExpander<'a> {
    pub params: &'a [TypeParam],
    pub i: &'a TypeParamInstantiation,
}

impl Fold<Type> for GenericExpander<'_> {
    fn fold(&mut self, ty: Type) -> Type {
        fn handle_lit(ty: &Type) -> Type {
            log::debug!("Expander: handle_lit {:?}", ty);

            match ty.normalize() {
                Type::Param(ty)
                    if ty.constraint.is_some() && is_literals(&ty.constraint.as_ref().unwrap()) =>
                {
                    return *ty.constraint.clone().unwrap();
                }
                _ => {}
            }

            ty.clone()
        }
        log::info!("Expander: expanding {:?}", ty);

        let ty = ty.fold_children(self);

        match ty.normalize() {
            Type::Ref(Ref {
                type_name: TsEntityName::Ident(Ident { ref sym, .. }),
                type_args: None,
                ..
            }) => {
                // Handle references to type parameters
                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == *sym {
                        return handle_lit(&self.i.params[idx]);
                    }
                }

                // Normal type reference
            }

            Type::Param(param) => {
                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == param.name {
                        return handle_lit(&self.i.params[idx]);
                    }
                }
            }

            _ => {}
        }

        ty
    }
}

/// This method returns true for types like `'foo'` and `'foo' | 'bar'`.
fn is_literals(ty: &Type) -> bool {
    match ty.normalize() {
        Type::Lit(_) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|v| is_literals(v)),
        _ => false,
    }
}
