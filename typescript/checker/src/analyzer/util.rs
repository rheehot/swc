use crate::ty::Type;
use std::borrow::Cow;
use swc_common::Spanned;
use swc_ecma_ast::*;

pub(super) trait PatExt {
    fn get_ty(&self) -> Option<&TsType>;
    fn set_ty(&mut self, ty: Option<Box<TsType>>);
}

impl PatExt for Pat {
    fn get_ty(&self) -> Option<&TsType> {
        match *self {
            Pat::Array(ArrayPat { ref type_ann, .. })
            | Pat::Assign(AssignPat { ref type_ann, .. })
            | Pat::Ident(Ident { ref type_ann, .. })
            | Pat::Object(ObjectPat { ref type_ann, .. })
            | Pat::Rest(RestPat { ref type_ann, .. }) => type_ann.as_ref().map(|ty| &*ty.type_ann),

            Pat::Expr(ref pat) => unreachable!("Cannot get type from Pat::Expr\n{:?}", pat),
        }
    }

    fn set_ty(&mut self, ty: Option<Box<TsType>>) {
        match *self {
            Pat::Array(ArrayPat {
                ref mut type_ann, ..
            })
            | Pat::Assign(AssignPat {
                ref mut type_ann, ..
            })
            | Pat::Ident(Ident {
                ref mut type_ann, ..
            })
            | Pat::Object(ObjectPat {
                ref mut type_ann, ..
            })
            | Pat::Rest(RestPat {
                ref mut type_ann, ..
            }) => {
                *type_ann = ty.map(|type_ann| TsTypeAnn {
                    span: type_ann.span(),
                    type_ann,
                })
            }

            Pat::Expr(ref pat) => {
                unreachable!("Cannot set type annottation for expression\n{:?}", pat)
            }
        }
    }
}

pub trait NormalizeMut<'b> {
    fn normalize_mut(&mut self) -> &mut Type<'b>;
}

impl<'b, T> NormalizeMut<'b> for Box<T>
where
    T: NormalizeMut<'b>,
{
    fn normalize_mut(&mut self) -> &mut Type<'b> {
        self.as_mut().normalize_mut()
    }
}

impl<'b, T> NormalizeMut<'b> for &'_ mut T
where
    T: NormalizeMut<'b>,
{
    fn normalize_mut(&mut self) -> &mut Type<'b> {
        (*self).normalize_mut()
    }
}

impl<'a, 'b> NormalizeMut<'b> for Cow<'a, Type<'b>> {
    fn normalize_mut(&mut self) -> &mut Type<'b> {
        let owned = match *self {
            Cow::Borrowed(borrowed) => {
                *self = Cow::Owned(borrowed.to_owned());
                match *self {
                    Cow::Borrowed(..) => unreachable!(),
                    Cow::Owned(ref mut owned) => owned,
                }
            }
            Cow::Owned(ref mut owned) => owned,
        };

        match *owned {
            Type::Static(s) => {
                *owned = s.ty.clone().owned().into_owned();
                owned
            }

            _ => owned,
        }
    }
}
