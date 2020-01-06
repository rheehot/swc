use super::Analyzer;
use crate::{
    analyzer::{scope::ScopeKind, util::ResultExt},
    errors::Error,
    ty::Type,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use swc_common::{Visit, VisitWith};
use swc_ecma_ast::*;

mod ambient_decl;
mod loops;

/// NOTE: We does **not** dig into with statements.
impl Visit<WithStmt> for Analyzer<'_, '_> {
    fn visit(&mut self, s: &WithStmt) {
        s.obj.visit_with(self);
    }
}

impl Visit<BlockStmt> for Analyzer<'_, '_> {
    fn visit(&mut self, s: &BlockStmt) {
        self.with_child(ScopeKind::Block, Default::default(), |analyzer| {
            s.stmts.visit_with(analyzer)
        })
    }
}

impl Visit<TsInterfaceDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, decl: &TsInterfaceDecl) {
        decl.visit_children(self);

        let ty = self
            .validate(decl)
            .store(&mut self.info.errors)
            .map(Type::from);

        self.register_type(
            decl.id.sym.clone(),
            ty.unwrap_or_else(|| Type::any(decl.span)),
        )
        .store(&mut self.info.errors);

        self.resolve_parent_interfaces(&decl.extends);
    }
}

impl Analyzer<'_, '_> {
    pub fn visit_stmts_for_return(&mut self, stmts: &[Stmt]) -> Result<Option<Type>, Error> {
        let types = {
            let mut v = ReturnTypeCollector {
                analyzer: &mut *self,
                types: Default::default(),
            };

            stmts.visit_with(&mut v);

            v.types
        };

        let mut buf = Vec::with_capacity(types.len());
        for ty in types {
            buf.extend(ty.store(&mut self.info.errors));
        }

        if buf.is_empty() {
            return Ok(None);
        }

        Ok(Some(Type::union(buf)))
    }

    /// Validate that parent interfaces are all resolved.
    pub fn resolve_parent_interfaces(&mut self, parents: &[TsExprWithTypeArgs]) {
        for parent in parents {
            // Verify parent interface
            let res: Result<_, _> = try {
                let type_args = try_opt!(parent.type_args.validate_with(self));
                self.type_of_ts_entity_name(parent.span, &parent.expr, type_args)?;
            };

            res.store(&mut self.info.errors);
        }
    }
}

struct ReturnTypeCollector<'a, A>
where
    A: Validate<Expr, Output = ValidationResult>,
{
    pub analyzer: &'a mut A,
    pub types: Vec<Result<Type, Error>>,
}

impl<A> Visit<ReturnStmt> for ReturnTypeCollector<'_, A>
where
    A: Validate<Expr, Output = ValidationResult>,
{
    fn visit(&mut self, s: &ReturnStmt) {
        if let Some(ty) = s.arg.validate_with(self.analyzer) {
            self.types.push(ty)
        }
    }
}

macro_rules! noop {
    ($T:ty) => {
        impl<A> Visit<$T> for ReturnTypeCollector<'_, A>
        where
            A: Validate<Expr, Output = ValidationResult>,
        {
            #[inline(always)]
            fn visit(&mut self, _: &$T) {}
        }
    };
}

noop!(Function);
noop!(ArrowExpr);
