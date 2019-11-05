use crate::analyzer::{expr::TypeOfMode, Analyzer};
use swc_common::{Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<UpdateExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, e: &UpdateExpr) {
        e.visit_children(self);

        match self.type_of_expr(&e.arg, TypeOfMode::LValue) {
            Ok(..) => {}
            Err(err) => self.info.errors.push(err),
        }
    }
}
