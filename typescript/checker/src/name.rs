use smallvec::{smallvec, SmallVec};
use std::convert::TryFrom;
use swc_atoms::{js_word, JsWord};
use swc_ecma_ast::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(SmallVec<[JsWord; 4]>);

impl From<&'_ Ident> for Name {
    #[inline]
    fn from(i: &Ident) -> Name {
        i.sym.clone().into()
    }
}

impl From<Ident> for Name {
    #[inline]
    fn from(i: Ident) -> Name {
        i.sym.into()
    }
}

impl From<&'_ JsWord> for Name {
    #[inline]
    fn from(v: &JsWord) -> Name {
        Name(smallvec![v.clone()])
    }
}

impl From<JsWord> for Name {
    #[inline]
    fn from(v: JsWord) -> Name {
        Name(smallvec![v])
    }
}

impl TryFrom<&'_ Expr> for Name {
    type Error = ();

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        match *e {
            Expr::Ident(ref i) => Ok(i.into()),
            // TODO
            _ => Err(()),
        }
    }
}

impl From<&'_ TsThisTypeOrIdent> for Name {
    fn from(ty: &TsThisTypeOrIdent) -> Self {
        match *ty {
            TsThisTypeOrIdent::TsThisType(..) => Name::from(js_word!("this")),
            TsThisTypeOrIdent::Ident(ref i) => Name::from(i),
        }
    }
}
