use crate::analyzer::{expr::TypeOfMode, Analyzer};
use swc_common::{Visit, VisitWith};
use swc_ecma_ast::*;

macro_rules! impl_for {
    ($T:ty) => {
        impl Visit<$T> for Analyzer<'_, '_> {
            fn visit(&mut self, n: &$T) {
                n.visit_children(self);

                self.check_lhs_of_for_loop(&n.left);
                if match n.left {
                    VarDeclOrPat::VarDecl(VarDecl { ref decls, .. }) => !decls.is_empty(),
                    _ => true,
                } {
                    self.check_rhs_of_for_loop(&n.right);
                }
            }
        }
    };
}

impl Analyzer<'_, '_> {
    fn check_lhs_of_for_loop(&mut self, e: &VarDeclOrPat) {
        // Check iterable
        let res: Result<(), _> = try {
            match *e {
                VarDeclOrPat::VarDecl(..) => {}
                VarDeclOrPat::Pat(ref pat) => match *pat {
                    Pat::Expr(ref e) => {
                        self.type_of_expr(&e, TypeOfMode::LValue)?;
                    }
                    Pat::Ident(ref i) => {
                        // TODO: verify
                        self.type_of_ident(i, TypeOfMode::LValue)?;
                    }
                    _ => {}
                },
            }
        };

        match res {
            Ok(..) => {}
            Err(err) => self.info.errors.push(err),
        }
    }

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

/// NOTE: We does **not** dig into with statements.
impl Visit<WithStmt> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &WithStmt) {
        match self.type_of(&node.obj) {
            Ok(..) => {}
            Err(err) => self.info.errors.push(err),
        }
    }
}

impl Visit<TsImportEqualsDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &TsImportEqualsDecl) {
        match node.module_ref {
            TsModuleRef::TsEntityName(ref e) => {
                match self.type_of_ts_entity_name(node.span, e, None) {
                    Ok(..) => {}
                    Err(err) => self.info.errors.push(err),
                }
            }
            _ => {}
        }
    }
}
