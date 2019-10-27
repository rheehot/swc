use super::{Analyzer, ScopeKind};
use crate::errors::Error;
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<GetterProp> for Analyzer<'_, '_> {
    fn visit(&mut self, prop: &GetterProp) {
        let entry = self.with_child(ScopeKind::Fn, Default::default(), |child| {
            child.return_type_span = prop.span();

            child
                .inferred_return_types
                .get_mut()
                .insert(prop.span(), Default::default());

            prop.visit_children(child);

            child
                .inferred_return_types
                .get_mut()
                .remove_entry(&prop.span())
                .unwrap_or_default()
        });

        if entry.1.is_empty() {
            // getter property must have return statements.
            self.info.errors.push(Error::GetterPropWithoutReturn {
                span: prop.key.span(),
            });
        }

        *self
            .inferred_return_types
            .get_mut()
            .entry(prop.span())
            .or_default() = entry.1;
    }
}
