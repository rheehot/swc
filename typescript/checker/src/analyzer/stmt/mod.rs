use crate::analyzer::Analyzer;
use swc_common::{Visit, VisitWith};
use swc_ecma_ast::*;

macro_rules! impl_for {
    ($T:ty) => {
        impl Visit<$T> for Analyzer<'_, '_> {
            fn visit(&mut self, n: &$T) {
                n.visit_children(self);

                let res: Result<(), _> = try {
                    self.type_of(&n.right)?;
                };

                match res {
                    Ok(..) => {}
                    Err(err) => self.info.errors.push(err),
                }
            }
        }
    };
}

impl_for!(ForOfStmt);
impl_for!(ForInStmt);
