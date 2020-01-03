use super::Analyzer;
use crate::{analyzer::scope::ScopeKind, errors::Error};
use swc_ecma_ast::*;

mod loops;

impl Analyzer<'_> {
    pub fn validate_stmt(&mut self, s: &Stmt) -> Result<(), Error> {
        match s {
            Stmt::Expr(s) => self.validate_expr_stmt(s),
            Stmt::Throw(s) => self.validate_throw_stmt(s),
            Stmt::With(s) => self.validate_with_stmt(s),
            Stmt::ForOf(s) => self.validate_for_of_stmt(s),
            Stmt::ForIn(s) => self.validate_for_in_stmt(s),
            Stmt::If(s) => self.validate_if_stmt(s),
            Stmt::Return(s) => self.validate_return_stmt(s),
        }
    }

    fn validate_expr_stmt(&mut self, s: &ExprStmt) -> Result<(), Error> {
        self.validate_expr(&s.expr)?;

        Ok(())
    }

    fn validate_throw_stmt(&mut self, s: &ThrowStmt) -> Result<(), Error> {
        self.validate_expr(&s.arg)?;

        Ok(())
    }

    /// NOTE: We does **not** dig into with statements.
    fn validate_with_stmt(&mut self, s: &WithStmt) -> Result<(), Error> {
        self.validate_expr(&s.obj)?;

        Ok(())
    }

    fn validate_block_stmt(&mut self, s: &BlockStmt) -> Result<(), Error> {
        self.with_child(ScopeKind::Block, Default::default(), |analyzer| {
            for stmt in &s.stmts {
                analyzer.validate_stmt(stmt)?;
            }

            Ok(())
        })
    }

    fn validate_return_stmt(&mut self, s: &ReturnStmt) -> Result<(), Error> {
        if let Some(ref arg) = s.arg {
            self.validate_expr(&arg)?;
        }

        Ok(())
    }

    fn validate_if_stmt(&mut self, s: &IfStmt) -> Result<(), Error> {
        let mut facts = Default::default();
        match self.detect_facts(&stmt.test, &mut facts) {
            Ok(()) => (),
            Err(err) => {
                self.info.errors.push(err);
                return stmt;
            }
        };
        let ends_with_ret = stmt.cons.ends_with_ret();
        let stmt = self.with_child(ScopeKind::Flow, facts.true_facts, |child| {
            stmt.fold_children(child)
        });
        if ends_with_ret {
            self.scope.facts.extend(facts.false_facts);
        }

        stmt
    }
}
