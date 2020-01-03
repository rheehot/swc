use crate::{
    errors::Error,
    legacy::Analyzer,
    swc_common::Spanned,
    ty::{Array, Type},
};
use swc_common::{Fold, FoldWith, Span};
use swc_ecma_ast::*;
use swc_ts_checker_macros::validator;

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
