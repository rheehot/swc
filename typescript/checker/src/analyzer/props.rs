use super::{Analyzer, ScopeKind};
use crate::errors::Error;
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

macro_rules! visit_method_like {
    ($a:expr, $node:expr) => {{
        let entry = $a.with_child(ScopeKind::Fn, Default::default(), |child| {
            child.return_type_span = $node.span();

            child
                .inferred_return_types
                .get_mut()
                .insert($node.span(), Default::default());

            $node.visit_children(child);

            child
                .inferred_return_types
                .get_mut()
                .remove_entry(&$node.span())
                .unwrap_or_default()
        });

        if entry.1.is_empty() {
            // getter property must have return statements.
            $a.info.errors.push(Error::GetterPropWithoutReturn {
                span: $node.key.span(),
            });
        }

        *$a.inferred_return_types
            .get_mut()
            .entry($node.span())
            .or_default() = entry.1;
    }};
}

impl Visit<GetterProp> for Analyzer<'_, '_> {
    fn visit(&mut self, prop: &GetterProp) {
        visit_method_like!(self, prop);
    }
}

// impl Visit<ClassMethod> for Analyzer<'_, '_> {
//     fn visit(&mut self, m: &ClassMethod) {
//         if m.kind == MethodKind::Getter {
//             visit_method_like!(self, m);
//         } else {
//             m.visit_children(self)
//         }
//     }
// }
