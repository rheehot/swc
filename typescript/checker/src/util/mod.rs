use crate::ty::{Class, FnParam, Intersection, Type, TypeElement, Union};
use swc_common::{Fold, FoldWith, Span, DUMMY_SP};
use swc_ecma_ast::*;

pub(crate) mod named;

pub trait EqIgnoreSpan {
    fn eq_ignore_span(&self, to: &Self) -> bool;
}

pub trait TypeEq<T = Self> {
    fn type_eq(&self, to: &T) -> bool;
}

macro_rules! impl_by_clone {
    ($T:ty) => {
        impl EqIgnoreSpan for $T {
            fn eq_ignore_span(&self, to: &Self) -> bool {
                self.clone().fold_with(&mut SpanRemover) == to.clone().fold_with(&mut SpanRemover)
            }
        }

        impl TypeEq<$T> for $T {
            fn type_eq(&self, to: &$T) -> bool {
                let l = self.clone().fold_with(&mut TypeEqHelper);
                let r = to.clone().fold_with(&mut TypeEqHelper);

                l == r
            }
        }
    };
}
impl_by_clone!(Type);
impl_by_clone!(Expr);
impl_by_clone!(TypeElement);
impl_by_clone!(TsLit);
impl_by_clone!(TsLitType);
impl_by_clone!(PropName);
impl_by_clone!(Class);

struct SpanRemover;
impl Fold<Span> for SpanRemover {
    fn fold(&mut self, _: Span) -> Span {
        DUMMY_SP
    }
}

struct TypeEqHelper;
impl Fold<Span> for TypeEqHelper {
    fn fold(&mut self, _: Span) -> Span {
        DUMMY_SP
    }
}
impl Fold<FnParam> for TypeEqHelper {
    fn fold(&mut self, node: FnParam) -> FnParam {
        FnParam {
            pat: Pat::Invalid(Invalid { span: DUMMY_SP }),
            ..node.fold_children(self)
        }
    }
}

impl<T> EqIgnoreSpan for Box<T>
where
    T: EqIgnoreSpan,
{
    fn eq_ignore_span(&self, to: &Self) -> bool {
        (**self).eq_ignore_span(&**to)
    }
}

impl<T> EqIgnoreSpan for Option<T>
where
    T: EqIgnoreSpan,
{
    fn eq_ignore_span(&self, to: &Self) -> bool {
        match (self.as_ref(), to.as_ref()) {
            (Some(l), Some(r)) => l.eq_ignore_span(r),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T> EqIgnoreSpan for Vec<T>
where
    T: EqIgnoreSpan,
{
    fn eq_ignore_span(&self, to: &Self) -> bool {
        if self.len() != to.len() {
            return false;
        }

        self.iter()
            .zip(to.iter())
            .all(|(l, r)| l.eq_ignore_span(&r))
    }
}

impl<T> TypeEq for Box<T>
where
    T: TypeEq,
{
    fn type_eq(&self, to: &Self) -> bool {
        (**self).type_eq(&**to)
    }
}

impl<T> TypeEq for Option<T>
where
    T: TypeEq,
{
    fn type_eq(&self, to: &Self) -> bool {
        match (self.as_ref(), to.as_ref()) {
            (Some(l), Some(r)) => l.type_eq(r),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T> TypeEq for Vec<T>
where
    T: TypeEq,
{
    fn type_eq(&self, to: &Self) -> bool {
        if self.len() != to.len() {
            return false;
        }

        self.iter().zip(to.iter()).all(|(l, r)| l.type_eq(&r))
    }
}

pub(crate) trait RemoveTypes {
    /// Removes falsy values from `self`.
    fn remove_falsy(self) -> Type;

    /// Removes truthy values from `self`.
    fn remove_truthy(self) -> Type;
}

impl RemoveTypes for Type {
    fn remove_falsy(self) -> Type {
        match self {
            Type::Keyword(TsKeywordType { kind, span }) => match kind {
                TsKeywordTypeKind::TsUndefinedKeyword | TsKeywordTypeKind::TsNullKeyword => {
                    return Type::never(span);
                }
                _ => {}
            },
            Type::Lit(TsLitType {
                lit:
                    TsLit::Bool(Bool {
                        value: false, span, ..
                    }),
                ..
            }) => return Type::never(span),

            Type::Union(u) => return u.remove_falsy(),
            Type::Intersection(i) => return i.remove_falsy(),
            _ => {}
        }

        self
    }

    fn remove_truthy(self) -> Type {
        match self {
            Type::Lit(TsLitType {
                lit: TsLit::Bool(Bool {
                    value: true, span, ..
                }),
                ..
            }) => return Type::never(span),

            Type::Union(u) => u.remove_truthy(),
            Type::Intersection(i) => i.remove_truthy(),
            _ => self,
        }
    }
}

impl RemoveTypes for Intersection {
    fn remove_falsy(self) -> Type {
        let types = self
            .types
            .into_iter()
            .map(|ty| ty.remove_falsy())
            .collect::<Vec<_>>();
        if types.iter().any(|ty| ty.is_never()) {
            return Type::never(self.span);
        }

        Intersection {
            span: self.span,
            types,
        }
        .into()
    }

    fn remove_truthy(self) -> Type {
        let types = self
            .types
            .into_iter()
            .map(|ty| ty.remove_truthy())
            .collect::<Vec<_>>();
        if types.iter().any(|ty| ty.is_never()) {
            return Type::never(self.span);
        }

        Intersection {
            span: self.span,
            types,
        }
        .into()
    }
}

impl RemoveTypes for Union {
    fn remove_falsy(self) -> Type {
        let types = self
            .types
            .into_iter()
            .map(|ty| ty.remove_falsy())
            .filter(|ty| !ty.is_never())
            .collect();
        Union {
            span: self.span,
            types,
        }
        .into()
    }

    fn remove_truthy(self) -> Type {
        let types = self
            .types
            .into_iter()
            .map(|ty| ty.remove_truthy())
            .filter(|ty| !ty.is_never())
            .collect();
        Union {
            span: self.span,
            types,
        }
        .into()
    }
}

impl<'a, T> RemoveTypes for Box<T>
where
    T: RemoveTypes,
{
    fn remove_falsy(self) -> Type {
        (*self).remove_falsy()
    }

    fn remove_truthy(self) -> Type {
        (*self).remove_truthy()
    }
}

pub(crate) trait EndsWithRet {
    /// Returns true if the statement ends with return, break, continue;
    fn ends_with_ret(&self) -> bool;
}

impl EndsWithRet for Stmt {
    /// Returns true if the statement ends with return, break, continue;
    fn ends_with_ret(&self) -> bool {
        match *self {
            Stmt::Return(..) | Stmt::Break(..) | Stmt::Continue(..) | Stmt::Throw(..) => true,
            Stmt::Block(ref stmt) => stmt.ends_with_ret(),
            _ => false,
        }
    }
}

impl EndsWithRet for BlockStmt {
    /// Returns true if the statement ends with return, break, continue;
    fn ends_with_ret(&self) -> bool {
        self.stmts.ends_with_ret()
    }
}

impl<T> EndsWithRet for Vec<T>
where
    T: EndsWithRet,
{
    /// Returns true if the statement ends with return, break, continue;
    fn ends_with_ret(&self) -> bool {
        match self.last() {
            Some(ref stmt) => stmt.ends_with_ret(),
            _ => false,
        }
    }
}
