use crate::{
    analyzer::{expr::TypeOfMode, Analyzer},
    errors::Error,
    swc_common::Spanned,
    ty::{Array, Type},
};
use swc_common::{Fold, FoldWith, Span};
use swc_ecma_ast::*;
use swc_ts_checker_macros::validator;

macro_rules! impl_for {
    ($T:ty) => {
        impl Fold<$T> for Analyzer<'_, '_> {
            fn fold(&mut self, n: $T) -> $T {
                let n = n.fold_children(self);

                self.check_lhs_of_for_loop(&n.left);
                if match n.left {
                    VarDeclOrPat::VarDecl(VarDecl { ref decls, .. }) => !decls.is_empty(),
                    _ => true,
                } {
                    self.check_rhs_of_for_loop(&n.right);
                }

                self.validate_for_loop(n.span, &n.left, &n.right);

                n
            }
        }
    };
}

impl Analyzer<'_, '_> {
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
}

impl_for!(ForOfStmt);
impl_for!(ForInStmt);

/// NOTE: We does **not** dig into with statements.
impl Fold<WithStmt> for Analyzer<'_, '_> {
    fn fold(&mut self, node: WithStmt) -> WithStmt {
        match self.type_of(&node.obj) {
            Ok(..) => {}
            Err(err) => self.info.errors.push(err),
        }

        node
    }
}

impl Fold<TsImportEqualsDecl> for Analyzer<'_, '_> {
    fn fold(&mut self, node: TsImportEqualsDecl) -> TsImportEqualsDecl {
        match node.module_ref {
            TsModuleRef::TsEntityName(ref e) => {
                match self.type_of_ts_entity_name(node.span, e, None) {
                    Ok(..) => {}
                    Err(err) => self.info.errors.push(err),
                }
            }
            _ => {}
        }

        node
    }
}

impl Fold<Stmt> for Analyzer<'_, '_> {
    fn fold(&mut self, stmt: Stmt) -> Stmt {
        log_fold!(stmt);
        let stmt = stmt.fold_children(self);

        match stmt {
            // Validate expression statements
            Stmt::Expr(ref s) => match self.type_of(&s.expr) {
                Ok(..) => {}
                Err(err) => {
                    self.info.errors.push(err);
                    return stmt;
                }
            },

            _ => {}
        }

        stmt
    }
}

impl Fold<ThrowStmt> for Analyzer<'_, '_> {
    fn fold(&mut self, s: ThrowStmt) -> ThrowStmt {
        log_fold!(s);

        let s = s.fold_children(self);

        match self
            .type_of(&s.arg)
            .and_then(|ty| self.expand_type(s.span, ty))
        {
            Ok(..) => {}
            Err(err) => {
                self.info.errors.push(err);
            }
        }

        s
    }
}
