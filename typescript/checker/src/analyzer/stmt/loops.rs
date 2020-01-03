use super::super::Analyzer;
use crate::{
    analyzer::expr::TypeOfMode,
    errors::Error,
    ty::{Array, Type},
};
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;
use swc_ts_checker_macros::validator;

impl Analyzer<'_> {
    #[validator]
    fn check_lhs_of_for_loop(&mut self, e: &VarDeclOrPat) {
        // Check iterable
        match *e {
            VarDeclOrPat::VarDecl(..) => {}
            VarDeclOrPat::Pat(ref pat) => match *pat {
                Pat::Expr(ref e) => {
                    self.type_of_expr(&e, TypeOfMode::LValue, None)?;
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
        self.type_of(e)?;
    }

    fn validate_for_loop(&mut self, span: Span, lhs: &VarDeclOrPat, rhs: &Expr) {
        let rty = match self.type_of(rhs).and_then(|ty| self.expand_type(span, ty)) {
            Ok(ty) => ty,
            Err(..) => return,
        };

        match lhs {
            VarDeclOrPat::Pat(Pat::Expr(ref l)) => {
                let lty = match self
                    .type_of_expr(&**l, TypeOfMode::LValue, None)
                    .and_then(|ty| self.expand_type(span, ty))
                {
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

    pub(super) fn validate_for_of_stmt(&mut self, s: &ForOfStmt) -> Result<(), Error> {
        // TODO: Visit children

        self.check_lhs_of_for_loop(&n.left);
        if match n.left {
            VarDeclOrPat::VarDecl(VarDecl { ref decls, .. }) => !decls.is_empty(),
            _ => true,
        } {
            self.check_rhs_of_for_loop(&n.right);
        }

        self.validate_for_loop(n.span, &n.left, &n.right);

        Ok(())
    }

    pub(super) fn validate_for_in_stmt(&mut self, s: &ForInStmt) -> Result<(), Error> {
        // TODO: Visit children

        self.check_lhs_of_for_loop(&n.left);
        if match n.left {
            VarDeclOrPat::VarDecl(VarDecl { ref decls, .. }) => !decls.is_empty(),
            _ => true,
        } {
            self.check_rhs_of_for_loop(&n.right);
        }

        self.validate_for_loop(n.span, &n.left, &n.right);

        Ok(())
    }
}
