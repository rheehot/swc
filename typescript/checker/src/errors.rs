use crate::ty::{Type, TypeElement};
use std::{ops::RangeInclusive, path::PathBuf};
use swc_atoms::JsWord;
use swc_common::{errors::Handler, Span, Spanned, DUMMY_SP};
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

    /// TS2539
    CannotAssignToNonVariable {
        span: Span,
    },

    /// TS2322
    AssignedWrapperToPrimitive {
        span: Span,
    },

    /// TS2322
    AccessibilityDiffers {
        span: Span,
    },

    /// TS2474
    InvalidInitInConstEnum {
        span: Span,
    },

    /// TS2352
    InvalidTupleCast {
        span: Span,
        left: Span,
        right: Span,
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
        name: JsWord,
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
        prop_ty: Option<Type>,
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

    /// TS2304
    NameNotFound {
        span: Span,
    },

    /// TS2378
    TS2378 {
        span: Span,
    },

    /// TS2475
    ConstEnumUsedAsVar {
        span: Span,
    },

    /// TS2476
    ConstEnumNonIndexAccess {
        span: Span,
    },

    // TS2493
    TupleIndexError {
        span: Span,
        len: u64,
        index: i64,
    },

    // TS2540
    InvalidLValue {
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
        fields: Vec<TypeElement>,
    },

    /// TS2322
    AssignFailed {
        span: Span,
        left: Type,
        right: Type,
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
        callee: Type,
    },

    NoCallSignature {
        span: Span,
        callee: Type,
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

    InvalidEnumInit {
        span: Span,
    },

    TS1016 {
        span: Span,
    },

    TS1063 {
        span: Span,
    },

    TS1094 {
        span: Span,
    },

    TS1095 {
        span: Span,
    },

    TS1168 {
        /// Span of offending computed property.
        span: Span,
    },

    TS1183 {
        span: Span,
    },

    TS1318 {
        span: Span,
    },

    TS1319 {
        span: Span,
    },

    TS2309 {
        span: Span,
    },

    TS2347 {
        span: Span,
    },

    TS2360 {
        span: Span,
    },

    TS2361 {
        span: Span,
    },

    TS2362 {
        span: Span,
    },

    TS2363 {
        span: Span,
    },

    TS2365 {
        span: Span,
    },

    TS2370 {
        span: Span,
    },

    TS2394 {
        span: Span,
    },

    TS1166 {
        span: Span,
    },

    TS1345 {
        span: Span,
    },

    TS2353 {
        span: Span,
    },

    TS2391 {
        span: Span,
    },

    TS2464 {
        span: Span,
    },

    TS2356 {
        span: Span,
    },

    TS2369 {
        span: Span,
    },

    TS2389 {
        span: Span,
    },

    TS2447 {
        span: Span,
    },

    TS2515 {
        span: Span,
    },

    TS2531 {
        span: Span,
    },

    TS2532 {
        span: Span,
    },

    TS2567 {
        span: Span,
    },

    TS2585 {
        span: Span,
    },

    TS2704 {
        span: Span,
    },

    /// `TS2358`
    InvalidLhsInInstanceOf {
        span: Span,
        /// Type of the lhs
        ty: Type,
    },

    /// `TS2359`
    InvalidRhsInInstanceOf {
        span: Span,
        /// Type of the rhs
        ty: Type,
    },
}

impl Error {
    #[cold]
    pub fn emit(self, h: &Handler) {
        let span = self.span();

        let mut err = match self {
            Error::Unimplemented { ref msg, .. } => {
                h.struct_err(&format!("unimplemented\n{}", msg))
            }
            Error::TS2378 { .. } => h.struct_err("A 'get' accessor must return a value"),
            Error::TS1094 { .. } => h.struct_err("An accessor cannot have type parameters"),
            Error::TS1183 { .. } => {
                h.struct_err("An implementation cannot be declared in ambient contexts")
            }
            Error::TS1318 { .. } => {
                h.struct_err("An abstract accessor cannot have an implementation")
            }
            Error::TS1095 { .. } => {
                h.struct_err("A 'set' accessor cannot have a return type annotation")
            }
            Error::TS2567 { .. } => h.struct_err(
                "Enum declarations can only merge with namespace or other enum declarations",
            ),
            _ => h.struct_err(&format!("{:#?}", self)),
        };
        err.set_span(span);

        //        err.code(DiagnosticId::Error(String::from(match self {
        //            _ => "",
        //        })));

        err.set_span(span).emit();
    }

    #[cold]
    pub fn flatten(vec: Vec<Error>) -> Vec<Error> {
        let mut buf = Vec::with_capacity(vec.len());

        for e in vec {
            match e {
                Error::Errors { errors, .. } => buf.extend(Self::flatten(errors)),
                _ => buf.push(e),
            }
        }

        buf
    }
}

impl From<Vec<Error>> for Error {
    fn from(errors: Vec<Error>) -> Self {
        Error::Errors {
            span: DUMMY_SP,
            errors,
        }
    }
}
