use crate::{
    analyzer::{expr::unwrap_paren, Analyzer},
    errors::Error,
    ty::Type,
};
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<UnaryExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &UnaryExpr) {
        node.visit_children(self);

        match node.op {
            op!("typeof") | op!("delete") | op!("void") => match self.type_of(&node.arg) {
                Ok(ref ty) => match ty.normalize() {
                    Type::EnumVariant(ref v) if node.op == op!("delete") => {
                        self.info.errors.push(Error::TS2704 {
                            span: node.arg.span(),
                        })
                    }

                    _ => {}
                },
                Err(err) => self.info.errors.push(err),
            },
            _ => {}
        }
    }
}
