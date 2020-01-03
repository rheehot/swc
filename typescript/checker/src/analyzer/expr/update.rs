use crate::{
    analyzer::{expr::TypeOfMode, Analyzer, ValidationResult},
    errors::Error,
    ty::Type,
};
use swc_common::{Fold, FoldWith, Spanned};
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    pub(super) fn validate_update_expr(&mut self, e: &UpdateExpr) -> ValidationResult {
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
            });

        store!(self, res);

        Ok(Type::Keyword(TsKeywordType {
            kind: TsKeywordTypeKind::TsNumberKeyword,
            span,
        })
        .into_cow())
    }
}
