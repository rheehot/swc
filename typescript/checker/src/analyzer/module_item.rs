use crate::{
    analyzer::{scope::ScopeKind, Analyzer},
    errors::Error,
};
use swc_common::{Fold, FoldWith};
use swc_ecma_ast::*;

impl Fold<TsModuleBlock> for Analyzer<'_, '_> {
    fn fold(&mut self, node: TsModuleBlock) -> TsModuleBlock {
        let old = self.top_level;
        self.top_level = false;
        let node = node.fold_children(self);
        self.top_level = old;

        node
    }
}

impl Fold<TsExportAssignment> for Analyzer<'_, '_> {
    fn fold(&mut self, export: TsExportAssignment) -> TsExportAssignment {
        if !self.top_level && !self.in_declare {
            self.info.errors.push(Error::TS1063 { span: export.span });
            // We don't verify expr
            export
        } else {
            let export = export.fold_children(self);
            self.export_default_expr(&export.expr);
            export
        }
    }
}

impl Fold<ExportDefaultExpr> for Analyzer<'_, '_> {
    fn fold(&mut self, export: ExportDefaultExpr) -> ExportDefaultExpr {
        if !self.top_level {
            self.info.errors.push(Error::TS1319 { span: export.span });
            // We don't verify expr
            export
        } else {
            let export = export.fold_children(self);
            self.export_default_expr(&export.expr);
            export
        }
    }
}
