use crate::{analyzer::Analyzer, errors::Error, ty::Type};
use swc_common::{Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<NewExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, e: &NewExpr) {
        e.visit_children(self);

        let res: Result<(), Error> = try {
            let callee_ty = self.type_of(&e.callee)?;
            match *callee_ty.normalize() {
                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                    ..
                }) if e.type_args.is_some() => Err(Error::TS2347 { span: e.span })?,
                _ => {}
            }
        };

        if let Err(err) = res {
            self.info.errors.push(err);
        }
    }
}

impl Visit<CallExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, e: &CallExpr) {
        e.visit_children(self);

        let res: Result<(), Error> = try {
            if let ExprOrSuper::Expr(ref callee) = e.callee {
                let callee_ty = self.type_of(&callee)?;
                match *callee_ty.normalize() {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) if e.type_args.is_some() => Err(Error::TS2347 { span: e.span })?,
                    _ => {}
                }
            }
        };

        if let Err(err) = res {
            self.info.errors.push(err);
        }
    }
}
