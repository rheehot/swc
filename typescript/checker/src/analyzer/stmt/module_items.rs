use super::super::Analyzer;
use crate::errors::Error;
use swc_atoms::js_word;
use swc_common::Fold;
use swc_ecma_ast::*;

impl Analyzer<'_> {
    pub(super) fn validate_ts_export_assignment(
        &mut self,
        s: &TsExportAssignment,
    ) -> Result<(), Error> {
        let ty = self.validate_expr(&s.expr)?;

        self.export_expr(js_word!("default"), ty)?;

        Ok(())
    }

    pub(super) fn validate_export_default_expr(
        &mut self,
        s: &ExportDefaultExpr,
    ) -> Result<(), Error> {
        let ty = self.validate_expr(&s.expr)?;

        self.export_expr(js_word!("default"), ty)?;

        Ok(())
    }
}
