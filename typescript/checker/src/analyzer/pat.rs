use crate::{analyzer::Analyzer, errors::Error, ty::Type};
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<RestPat> for Analyzer<'_, '_> {
    fn visit(&mut self, p: &RestPat) {
        p.visit_children(self);

        if let Some(ref type_ann) = p.type_ann {
            self.analyze(|a| {
                let ty = a.expand_type(p.span(), Type::from(type_ann.clone().type_ann).owned())?;

                match *ty.normalize() {
                    Type::Array(..)
                    | Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => Err(Error::TS2370 { span: p.dot3_token })?,
                    _ => {}
                }

                Ok(())
            });
        }
    }
}
