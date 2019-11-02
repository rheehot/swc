use super::{Analyzer, ScopeKind};
use crate::errors::Error;
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<GetterProp> for Analyzer<'_, '_> {
    fn visit(&mut self, n: &GetterProp) {
        let entry = self.with_child(ScopeKind::Fn, Default::default(), |child| {
            child.return_type_span = n.span();

            child
                .inferred_return_types
                .get_mut()
                .insert(n.span(), Default::default());

            n.visit_children(child);

            child
                .inferred_return_types
                .get_mut()
                .remove_entry(&n.span())
                .unwrap_or_default()
        });

        if entry.1.is_empty() {
            // getter property must have return statements.
            self.info
                .errors
                .push(Error::GetterPropWithoutReturn { span: n.key.span() });
        }

        *self
            .inferred_return_types
            .get_mut()
            .entry(n.span())
            .or_default() = entry.1;
    }
}

impl Visit<ClassMethod> for Analyzer<'_, '_> {
    fn visit(&mut self, n: &ClassMethod) {
        if n.kind == MethodKind::Getter {
            let entry = self.with_child(ScopeKind::Fn, Default::default(), |child| {
                child.return_type_span = n.span();

                child
                    .inferred_return_types
                    .get_mut()
                    .insert(n.span(), Default::default());

                n.function.visit_children(child);

                child
                    .inferred_return_types
                    .get_mut()
                    .remove_entry(&n.span())
                    .unwrap_or_default()
            });

            if entry.1.is_empty() {
                // getter property must have return statements.
                self.info
                    .errors
                    .push(Error::GetterPropWithoutReturn { span: n.key.span() });
            }

            *self
                .inferred_return_types
                .get_mut()
                .entry(n.span())
                .or_default() = entry.1;
        } else {
            n.visit_children(self)
        }
    }
}
