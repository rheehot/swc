use super::Analyzer;
use crate::analyzer::scope::ScopeKind;
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

        self.scope
            .register_type(decl.id.sym.clone(), decl.clone().into());

        self.resolve_parent_interfaces(&decl.extends);
    }
}

impl Analyzer<'_, '_> {
    /// Validate that parent interfaces are all resolved.
    pub(super) fn resolve_parent_interfaces(&mut self, parents: &[TsExprWithTypeArgs]) {
        for parent in parents {
            // Verify parent interface
            let res =
                self.type_of_ts_entity_name(parent.span, &parent.expr, parent.type_args.as_ref());
            res.store(&mut self.info.errors);
        }
    }
}
