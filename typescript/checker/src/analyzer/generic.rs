use crate::{
    errors::Error,
    ty::{Conditional, Type, TypeParamDecl},
    util::IntoCow,
};
use std::borrow::Cow;
use swc_ecma_ast::*;

pub(super) fn expand_type_params<'a, 'b>(
    i: Option<&TsTypeParamInstantiation>,
    decl: &TypeParamDecl,
    ty: &'b Type<'a>,
) -> Result<Cow<'b, Type<'a>>, Error> {
    match *ty.normalize() {
        Type::Simple(ref sty) => match **sty {
            TsType::TsTypeRef(TsTypeRef {
                type_name: TsEntityName::Ident(Ident { ref sym, .. }),
                ..
            }) => {
                // Handle references to type parameters
                for (idx, p) in decl.params.iter().enumerate() {
                    if p.name == *sym {
                        if let Some(i) = i {
                            return Ok(Type::from(i.params[idx].clone()).into_cow());
                        }
                    }
                }

                // Normal type reference
                return Ok(Cow::Borrowed(ty));
            }
            _ => {}
        },

        Type::Conditional(Conditional {
            ref check_type,
            ref extends_type,
            ref true_type,
            ref false_type,
            ..
        }) => {
            let check_type = expand_type_params(i, decl, check_type)?;

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
        _ => {}
    }

    unimplemented!("expand_type_params({:#?})", ty)
}
