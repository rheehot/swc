use super::Analyzer;
use crate::{
    ty::{
        self, Conditional, FnParam, Mapped, Ref, Tuple, Type, TypeParam, TypeParamDecl,
        TypeParamInstantiation,
    },
    validator::ValidateWith,
    ValidationResult,
};
use swc_common::{Fold, FoldWith, Spanned};
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    pub(super) fn infer_arg_types(
        &mut self,
        type_params: &[TypeParam],
        params: &[FnParam],
        args: &[ExprOrSpread],
    ) -> ValidationResult<TypeParamInstantiation> {
        let mut arg_types = vec![];

        for arg in args {
            match *arg {
                ExprOrSpread {
                    spread: Some(..), ..
                } => unimplemented!("type parameter inference with spread element"),
                ExprOrSpread {
                    spread: None,
                    ref expr,
                    ..
                } => {
                    arg_types.push(expr.validate_with(self)?);
                }
            }
        }

        unimplemented!("infer_arg_types()")
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
        let ty = ty.fold_children(self);

        match ty.normalize() {
            Type::Ref(Ref {
                type_name: TsEntityName::Ident(Ident { ref sym, .. }),
                type_params: None,
                ..
            }) => {
                // Handle references to type parameters
                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == *sym {
                        return Type::from(self.i.params[idx].clone());
                    }
                }

                // Normal type reference
            }

            Type::Param(param) => {
                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == param.name {
                        return Type::from(self.i.params[idx].clone());
                    }
                }
            }

            _ => {}
        }

        ty
    }
}
