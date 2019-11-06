use super::{expr::TypeOfMode, ty::Type, Analyzer, ScopeKind};
use crate::{errors::Error, ty::TypeRefExt};
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    pub(super) fn validate_prop_name(&mut self, p: &PropName) {
        let span = p.span();

        analyze!(self, {
            match p {
                PropName::Computed(ref expr) => {
                    let ty = self.type_of(&expr)?;
                    let ty = ty.generalize_lit();
                    match *ty.normalize() {
                        Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        })
                        | Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsStringKeyword,
                            ..
                        })
                        | Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                            ..
                        })
                        | Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsSymbolKeyword,
                            ..
                        }) => {}
                        _ => Err(Error::TS2464 { span })?,
                    }
                }
                _ => {}
            }
        });
    }
}

impl Visit<Prop> for Analyzer<'_, '_> {
    fn visit(&mut self, n: &Prop) {
        n.visit_children(self);

        match n {
            Prop::Shorthand(ref i) => {
                analyze!(self, {
                    // TODO: Check if RValue is correct
                    self.type_of_ident(&i, TypeOfMode::RValue)?;
                });
            }
            _ => {}
        }
    }
}

impl Visit<KeyValueProp> for Analyzer<'_, '_> {
    fn visit(&mut self, n: &KeyValueProp) {
        n.visit_children(self);

        self.validate_prop_name(&n.key);
    }
}

impl Visit<MethodProp> for Analyzer<'_, '_> {
    fn visit(&mut self, n: &MethodProp) {
        n.visit_children(self);

        self.validate_prop_name(&n.key);
    }
}

impl Visit<GetterProp> for Analyzer<'_, '_> {
    fn visit(&mut self, n: &GetterProp) {
        self.validate_prop_name(&n.key);

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

impl Visit<TsPropertySignature> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &TsPropertySignature) {
        node.visit_children(self);

        if node.computed {
            self.validate_computed_prop_key(node.span(), &node.key);
        }
    }
}
