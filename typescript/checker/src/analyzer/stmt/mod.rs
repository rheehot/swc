use super::Analyzer;
use crate::{
    analyzer::{scope::ScopeKind, util::ResultExt},
    errors::Error,
    ty::Type,
    validator::{Validate, ValidateWith},
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

        self.scope.register_type(
            decl.id.sym.clone(),
            ty.unwrap_or_else(|| Type::any(decl.span)),
        );

        self.resolve_parent_interfaces(&decl.extends);
    }
}

impl Analyzer<'_, '_> {
    pub fn visit_stmts_for_return(&mut self, stmts: &[Stmt]) -> Result<Option<Type>, Error> {
        unimplemented!("visit_stmts_for_return")
    }

    /// Validate that parent interfaces are all resolved.
    pub fn resolve_parent_interfaces(&mut self, parents: &[TsExprWithTypeArgs]) {
        for parent in parents {
            // Verify parent interface
            let res: Result<_, _> = try {
                self.type_of_ts_entity_name(
                    parent.span,
                    &parent.expr,
                    try_opt!(parent.type_args.validate_with(self)),
                );
            };

            res.store(&mut self.info.errors);
        }
    }
}
