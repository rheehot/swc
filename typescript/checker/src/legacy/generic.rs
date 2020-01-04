use super::Analyzer;
use crate::{
    errors::Error,
    ty::{self, Conditional, Mapped, Tuple, Type, TypeParam, TypeParamDecl},
    util::IntoCow,
};
use std::borrow::Cow;
use swc_common::{Fold, FoldWith, Spanned};
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    pub(super) fn infer_arg_types(
        &self,
        args: &[ExprOrSpread],
        _type_params: &TypeParamDecl,
        _params: &[TsFnParam],
    ) -> Result<TsTypeParamInstantiation, Error> {
        let mut arg_types = Vec::with_capacity(args.len());

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
                    arg_types.push(self.type_of(expr)?);
                }
            }
        }

        unimplemented!("infer_arg_types()")
    }

    pub(super) fn expand_type_params<'a, 'b>(
        &self,
        i: &TsTypeParamInstantiation,
        decl: &TypeParamDecl,
        mut ty: Cow<'b, Type>,
    ) -> Result<Cow<'b, Type>, Error> {
        match *ty.normalize() {
            Type::TypeLit(..) | Type::Keyword(..) | Type::Lit(..) => return Ok(ty),

            Type::Simple(ref sty) => match **sty {
                TsType::TsTypeRef(TsTypeRef {
                    type_name: TsEntityName::Ident(Ident { ref sym, .. }),
                    ..
                }) => {
                    // Handle references to type parameters
                    for (idx, p) in decl.params.iter().enumerate() {
                        if p.name == *sym {
                            return Ok(Type::from(i.params[idx].clone()).owned());
                        }
                    }

                    // Normal type reference
                    return Ok(ty);
                }

                _ => {}
            },

            Type::Operator(ref op) => {
                let expanded = self.expand_type_params(i, decl, Cow::Borrowed(&op.ty))?;

                if let Cow::Owned(expanded) = expanded {
                    match ty.normalize_mut() {
                        Type::Operator(ref mut operator) => {
                            operator.ty = box Cow::Owned(expanded);
                        }

                        ref ty => unreachable!("{:#?}", ty),
                    }
                }

                return Ok(ty);
            }

            Type::Conditional(Conditional {
                ref check_type,
                ref extends_type,
                ref true_type,
                ref false_type,
                ..
            }) => {
                let check_type = self.expand_type_params(i, decl, Cow::Borrowed(check_type))?;
                if let Some(v) = self.extends(&check_type, &extends_type) {
                    return Ok(if v {
                        *true_type.clone()
                    } else {
                        *false_type.clone()
                    });
                }

                //
                unimplemented!(
                    "expanding conditional type.\nParams: {:#?}\nParam decls: {:#?}\nCheck Type: \
                     {:#?}\nextends_type: {:#?}\nTrue type: {:#?}\nFalse type: {:#?}",
                    i,
                    decl,
                    check_type,
                    extends_type,
                    true_type,
                    false_type
                )
            }

            Type::Mapped(Mapped {
                readonly: _,
                optional: _,
                ty: _,
                ref type_param,
                ..
            }) => {
                let type_param = self.expand_type_param(i, decl, type_param)?;

                // TODO:
                //
                //     type T50<T> = { [P in keyof T]: number };
                //     type T51 = T50<any>;  // { [x: string]: number }
                //     type T52 = T50<unknown>;  // {}

                if let Cow::Owned(type_param) = type_param {
                    match ty.normalize_mut() {
                        Type::Mapped(ref mut ty) => {
                            ty.type_param = type_param;
                        }

                        ref ty => unreachable!("{:#?}", ty),
                    }
                }

                return Ok(ty);
            }

            Type::Tuple(Tuple { ref types, .. }) => {
                let mut buf = vec![];

                for (idx, t) in types.into_iter().enumerate() {
                    let t = self.expand_type_params(i, decl, Cow::Borrowed(&t))?;
                    if let Cow::Owned(t) = t {
                        buf.push((idx, t))
                    }
                }

                for (idx, t) in buf {
                    match ty.normalize_mut() {
                        Type::Tuple(ref mut tuple) => {
                            tuple.types[idx] = Cow::Owned(t);
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
                let mut v = GenericExpander { decl, i };
                let ret_ty = ret_ty.clone().fold_with(&mut v);
                let params = params.clone().fold_with(&mut v);
                let type_params = type_params.clone().fold_with(&mut v);
                return Ok(Type::Function(ty::Function {
                    span,
                    params,
                    type_params,
                    ret_ty,
                })
                .into_cow());
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

    fn expand_type_param<'a, 'b>(
        &self,
        i: &TsTypeParamInstantiation,
        decl: &TypeParamDecl,
        type_param: &'a TypeParam<'b>,
    ) -> Result<Cow<'a, TypeParam<'b>>, Error> {
        let mut tp = Cow::Borrowed(type_param);

        if let Some(ref c) = type_param.constraint {
            let c = self.expand_type_params(i, decl, Cow::Borrowed(c))?;

            if let Cow::Owned(c) = c {
                tp.to_mut().constraint = Some(box c.into_cow());
            }
        }

        Ok(tp)
    }

    /// Returns `Some(true)` if `child` extends `parent`.
    fn extends(&self, child: &Type, parent: &Type) -> Option<bool> {
        let span = child.span();

        match self.assign(parent, child, span) {
            Ok(()) => return Some(true),
            _ => return Some(false),
        }
    }
}

struct GenericExpander<'a, 'b> {
    decl: &'a TypeParamDecl<'b>,
    i: &'a TsTypeParamInstantiation,
}

impl<'a, 'b> Fold<Cow<'a, Type<'b>>> for GenericExpander<'_, '_> {
    fn fold(&mut self, ty: Cow<'a, Type<'b>>) -> Cow<'a, Type<'b>> {
        let ty = ty.fold_children(self);

        match ty.normalize() {
            Type::Simple(ref sty) => match **sty {
                TsType::TsTypeRef(TsTypeRef {
                    type_name: TsEntityName::Ident(Ident { ref sym, .. }),
                    type_params: None,
                    ..
                }) => {
                    // Handle references to type parameters
                    for (idx, p) in self.decl.params.iter().enumerate() {
                        if p.name == *sym {
                            return Type::from(self.i.params[idx].clone());
                        }
                    }

                    // Normal type reference
                    return ty;
                }
                _ => return ty,
            },
            _ => return ty,
        }
    }
}
