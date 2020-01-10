use smallvec::{smallvec, SmallVec};
use std::convert::TryFrom;
use swc_atoms::{js_word, JsWord};
use swc_ecma_ast::*;

type Inner = SmallVec<[JsWord; 4]>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Inner);

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

impl From<TsEntityName> for Name {
    fn from(n: TsEntityName) -> Self {
        fn expand(buf: &mut Inner, n: TsEntityName) {
            match n {
                TsEntityName::Ident(i) => buf.push(i.sym),

                TsEntityName::TsQualifiedName(box q) => {
                    expand(buf, q.left);
                    buf.push(q.right.sym);
                }
            }
        }

        let mut buf = Inner::default();
        expand(&mut buf, n);
        Self(buf)
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
