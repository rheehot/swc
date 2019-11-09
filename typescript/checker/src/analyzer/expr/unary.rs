use crate::{
    analyzer::{expr::unwrap_paren, Analyzer},
    errors::Error,
};
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<UnaryExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &UnaryExpr) {
        node.visit_children(self);

        match node.op {
            op!("typeof") | op!("delete") | op!("void") => match self.type_of(&node.arg) {
                Ok(..) => {}
                Err(err) => self.info.errors.push(err),
            },
            _ => {}
        }
    }
}
