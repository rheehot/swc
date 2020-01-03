use super::Analyzer;
use crate::{
    analyzer::util::{Comparator, ResultExt},
    errors::Error,
    ty::{Type, TypeParamInstantiation, TypeRef},
    util::{EqIgnoreSpan, IntoCow},
    ValidationResult,
};
use swc_common::Spanned;
use swc_ecma_ast::*;

mod bin;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TypeOfMode {
    /// Used for l-values.
    ///
    /// This is used to allow
    ///
    /// ```ts
    /// type Num = { '0': string } | { [n: number]: number }
    /// declare var num: Num
    /// num[0] = 1
    /// num['0'] = 'ok'
    /// ```
    LValue,
    /// Use for r-values.
    RValue,
}

impl Analyzer<'_> {
    pub(super) fn validate_expr(&mut self, e: &Expr) -> ValidationResult {
        self.validate_expr_with_extra(e, TypeOfMode::RValue, None)
    }

    pub(super) fn validate_expr_with_extra(
        &mut self,
        e: &Expr,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
    ) -> Result<TypeRef, Error> {
        match e {
            Expr::Update(e) => self.validate_update_expr(e),
        }
    }

    fn validate_update_expr(&mut self, e: &UpdateExpr) -> ValidationResult {
        let span = e.span;

        let res = self
            .validate_expr_with_extra(&e.arg, TypeOfMode::LValue, None)
            .and_then(|ty| self.expand_type(span, ty))
            .and_then(|ty| match *ty.normalize() {
                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Lit(TsLitType {
                    lit: TsLit::Str(..),
                    ..
                })
                | Type::Array(..) => Err(Error::TS2356 { span: e.arg.span() }),

                _ => Ok(()),
            })
            .store(&mut self.info.errors);

        Ok(Type::Keyword(TsKeywordType {
            kind: TsKeywordTypeKind::TsNumberKeyword,
            span,
        })
        .into_cow())
    }
}
