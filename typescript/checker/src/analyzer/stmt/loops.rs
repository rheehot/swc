use super::super::Analyzer;
use crate::{
    analyzer::{expr::TypeOfMode, ScopeKind},
    errors::Error,
    ty::{Array, Type},
    validator::Validate,
    ValidationResult,
};
use macros::validator_method;
use swc_common::{Span, Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    #[validator_method]
    fn check_lhs_of_for_loop(&mut self, e: &VarDeclOrPat) {
        match *e {
            VarDeclOrPat::VarDecl(ref v) => {
                // Store variables
                v.visit_with(self);
            }
            VarDeclOrPat::Pat(ref pat) => match *pat {
                Pat::Expr(ref e) => {
                    self.validate_expr(&e, TypeOfMode::LValue, None)?;
                }
                Pat::Ident(ref i) => {
                    // TODO: verify
                    self.type_of_ident(i, TypeOfMode::LValue, None)?;
                }
                _ => {}
            },
        }
    }

    #[validator_method]
    fn check_rhs_of_for_loop(&mut self, e: &Expr) {
        // Check iterable
        self.validate(e)?;
    }

    fn validate_for_loop(&mut self, span: Span, lhs: &VarDeclOrPat, rhs: &Expr) {
        let rty = match self.validate(rhs) {
            Ok(ty) => ty,
            Err(..) => return,
        };

        match lhs {
            VarDeclOrPat::Pat(Pat::Expr(ref l)) => {
                let lty = match self.validate_expr(&**l, TypeOfMode::LValue, None) {
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

    #[validator_method]
    fn check_for_of_in_loop(&mut self, span: Span, left: &VarDeclOrPat, rhs: &Expr) {
        self.with_child(
            ScopeKind::Flow,
            Default::default(),
            |child| -> ValidationResult<()> {
                child.check_lhs_of_for_loop(left);
                if match left {
                    VarDeclOrPat::VarDecl(VarDecl { ref decls, .. }) => !decls.is_empty(),
                    _ => true,
                } {
                    child.check_rhs_of_for_loop(&rhs);
                }

                child.validate_for_loop(span, &left, &rhs);

                Ok(())
            },
        )?;
    }
}

impl Visit<ForInStmt> for Analyzer<'_, '_> {
    fn visit(&mut self, s: &ForInStmt) {
        self.check_for_of_in_loop(s.span, &s.left, &s.right)
    }
}

impl Visit<ForOfStmt> for Analyzer<'_, '_> {
    fn visit(&mut self, s: &ForOfStmt) {
        self.check_for_of_in_loop(s.span, &s.left, &s.right)
    }
}
