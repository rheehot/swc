use crate::analyzer::Analyzer;
use swc_common::{Visit, VisitWith};
use swc_ecma_ast::*;

macro_rules! impl_for {
    ($T:ty) => {
        impl Visit<$T> for Analyzer<'_, '_> {
            fn visit(&mut self, n: &$T) {
                n.visit_children(self);

                self.check_rhs_of_for_loop(&n.right);
            }
        }
    };
}

impl Analyzer<'_, '_> {
    fn check_rhs_of_for_loop(&mut self, e: &Expr) {
        // Check iterable
        let res: Result<(), _> = try {
            self.type_of(e)?;
        };

        match res {
            Ok(..) => {}
            Err(err) => self.info.errors.push(err),
        }
    }
}

impl_for!(ForOfStmt);
impl_for!(ForInStmt);
