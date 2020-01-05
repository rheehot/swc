use super::super::Analyzer;
use crate::{
    analyzer::expr::TypeOfMode,
    errors::Error,
    ty::{Array, Type},
};
use swc_common::{Span, Spanned, Visit, VisitWith};
use swc_ecma_ast::*;
use swc_ts_checker_macros::validator;

impl Analyzer<'_, '_> {
    #[validator]
    fn check_lhs_of_for_loop(&mut self, e: &VarDeclOrPat) {
        // Check iterable
        match *e {
            VarDeclOrPat::VarDecl(..) => {}
            VarDeclOrPat::Pat(ref pat) => match *pat {
                Pat::Expr(ref e) => {
                    self.visit_expr_with_extra(&e, TypeOfMode::LValue, None)?;
                }
                Pat::Ident(ref i) => {
                    // TODO: verify
                    self.type_of_ident(i, TypeOfMode::LValue)?;
                }
                _ => {}
            },
        }
    }

    #[validator]
    fn check_rhs_of_for_loop(&mut self, e: &Expr) {
        // Check iterable
        self.visit_expr(e)?;
    }

    fn validate_for_loop(&mut self, span: Span, lhs: &VarDeclOrPat, rhs: &Expr) {
        let rty = match self.visit_expr(rhs) {
            Ok(ty) => ty,
            Err(..) => return,
        };

        match lhs {
            VarDeclOrPat::Pat(Pat::Expr(ref l)) => {
                let lty = match self.visit_expr_with_extra(&**l, TypeOfMode::LValue, None) {
                    Ok(ty) => ty,
                    Err(..) => return,
                };

                println!("FOO\nL: {:?}", lty);
                match self.assign(
                    &Type::Array(Array {
                        span,
                        elem_type: box lty,
                    }),
                    &rty,
                    lhs.span(),
                ) {
                    Ok(..) => {}
                    Err(err) => self.info.errors.push(err),
                }
            }
            _ => {}
        }
    }
}

impl Visit<ForInStmt> for Analyzer<'_, '_> {
    fn visit(&mut self, s: &ForInStmt) {
        s.visit_children(self);

        self.check_lhs_of_for_loop(&s.left);
        if match s.left {
            VarDeclOrPat::VarDecl(VarDecl { ref decls, .. }) => !decls.is_empty(),
            _ => true,
        } {
            self.check_rhs_of_for_loop(&s.right);
        }

        self.validate_for_loop(s.span, &s.left, &s.right);
    }
}

impl Visit<ForOfStmt> for Analyzer<'_, '_> {
    fn visit(&mut self, s: &ForOfStmt) {
        s.visit_children(self);

        self.check_lhs_of_for_loop(&s.left);
        if match s.left {
            VarDeclOrPat::VarDecl(VarDecl { ref decls, .. }) => !decls.is_empty(),
            _ => true,
        } {
            self.check_rhs_of_for_loop(&s.right);
        }

        self.validate_for_loop(s.span, &s.left, &s.right);
    }
}
