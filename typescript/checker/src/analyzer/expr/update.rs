use crate::{
    analyzer::{expr::TypeOfMode, Analyzer},
    errors::Error,
    ty::Type,
};
use swc_common::{Fold, FoldWith, Spanned};
use swc_ecma_ast::*;

impl Fold<UpdateExpr> for Analyzer<'_, '_> {
    fn fold(&mut self, e: UpdateExpr) -> UpdateExpr {
        log_fold!(e);

        let e = e.fold_children(self);

        let mut errors = vec![];

        match self
            .type_of_expr(&e.arg, TypeOfMode::LValue)
            .and_then(|ty| self.expand_type(ty.span(), ty))
        {
            Ok(ty) => match *ty.normalize() {
                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Lit(TsLitType {
                    lit: TsLit::Str(..),
                    ..
                })
                | Type::Array(..) => errors.push(Error::TS2356 { span: e.arg.span() }),

                _ => {}
            },
            Err(err) => errors.push(err),
        }

        self.info.errors.extend(errors);

        e
    }
}
