use crate::ty::{Type, TypeElement};
use std::{ops::RangeInclusive, path::PathBuf};
use swc_atoms::JsWord;
use swc_common::{errors::Handler, Span, Spanned};
use swc_ecma_ast::Expr;

#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum Error {
    ReturnRequired {
        /// Span of the return type.
        span: Span,
    },

    ConstructorRequired {
        span: Span,
        lhs: Span,
        rhs: Span,
    },

    /// TS2474
    InvalidInitInConstEnum {
        span: Span,
    },

    /// TS2367
    NoOverlap {
        span: Span,
        value: bool,
        left: Span,
        right: Span,
    },

    ReadOnly {
        span: Span,
    },

    ImplicitAny {
        span: Span,
    },

    Errors {
        span: Span,
        errors: Vec<Error>,
    },

    RedclaredVarWithDifferentType {
        span: Span,
    },

    NoSuchType {
        span: Span,
        name: JsWord,
    },

    NoSuchVar {
        span: Span,
        name: JsWord,
    },

    DuplicateName {
        span: Span,
    },

    UselessSeqExpr {
        span: Span,
    },

    ClassPropertyInitRequired {
        span: Span,
    },

    ReferencedInInit {
        span: Span,
    },

    NotGeneric {
        span: Span,
    },

    Unknown {
        span: Span,
    },

    NoSuchProperty {
        span: Span,
        prop: Option<Expr>,
    },

    TooManyTupleElements {
        span: Span,
    },

    NotTuple {
        span: Span,
    },

    NotVariable {
        // Span of rhs
        span: Span,
        left: Span,
    },

    /// TS2476
    ConstEnumNonIndexAccess {
        span: Span,
    },

    Unimplemented {
        span: Span,
        msg: String,
    },

    ResolvedFailed {
        span: Span,
        base: PathBuf,
        src: JsWord,
    },

    MissingFields {
        span: Span,
        fields: Vec<TypeElement<'static>>,
    },

    AssignFailed {
        span: Span,
        left: Type<'static>,
        right: Type<'static>,
        cause: Vec<Error>,
    },

    /// a or b or c
    UnionError {
        span: Span,
        errors: Vec<Error>,
    },

    IntersectionError {
        span: Span,
        error: Box<Error>,
    },

    CannotAssingToThis {
        span: Span,
    },

    MayBeUndefined {
        /// Span of the variable
        span: Span,
    },

    UndefinedSymbol {
        span: Span,
    },

    ModuleLoadFailed {
        /// Span of the import statement.
        span: Span,
        errors: Vec<Error>,
    },

    NoSuchExport {
        span: Span,
        items: Vec<(JsWord, Span)>,
    },

    NoNewSignature {
        span: Span,
        callee: Type<'static>,
    },

    NoCallSignature {
        span: Span,
        callee: Type<'static>,
    },

    WrongTypeParams {
        /// Span of caller.
        span: Span,
        /// Span of callee.
        callee: Span,
        expected: RangeInclusive<usize>,
        actual: usize,
    },

    WrongParams {
        /// Span of caller.
        span: Span,
        /// Span of callee.
        callee: Span,
        expected: RangeInclusive<usize>,
        actual: usize,
    },
}

impl Error {
    pub fn emit(self, h: &Handler) {
        let span = self.span();

        let mut err = match self {
            Error::Unimplemented { ref msg, .. } => {
                h.struct_err(&format!("unimplemented\n{}", msg))
            }
            _ => h.struct_err(&format!("{:#?}", self)),
        };

        err.set_span(span).emit();
    }

    #[cold]
    pub fn flatten(vec: Vec<Error>) -> Vec<Error> {
        let mut buf = Vec::with_capacity(vec.len());

        for e in vec {
            match e {
                Error::Errors { errors, .. } => buf.extend(errors),
                _ => buf.push(e),
            }
        }

        buf
    }
}
