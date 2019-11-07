use super::{expr::TypeOfMode, ty::Type, Analyzer, ScopeKind};
use crate::{analyzer::ComputedPropMode, errors::Error, ty::TypeRefExt};
use swc_atoms::js_word;
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<ComputedPropName> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &ComputedPropName) {
        // TODO: check if it's class or object literal
        node.visit_children(self);

        let span = node.span;

        analyze!(self, {
            let mut errors = vec![];
            let ty = match self.type_of(&node.expr) {
                Ok(ty) => ty,
                Err(err) => {
                    errors.push(err);
                    // TODO: Change this to something else (maybe any)
                    Type::unknown(span).owned()
                }
            };
            match self.computed_prop_mode {
                ComputedPropMode::Class => {
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
                        _ => match *node.expr {
                            Expr::Ident(ref i)
                                if i.sym == js_word!("public")
                                    || i.sym == js_word!("protected")
                                    || i.sym == js_word!("private")
                                    || i.sym == js_word!("readonly") => {}
                            _ => {
                                errors.push(Error::TS2464 { span });
                            }
                        },
                    }
                }
                _ => {}
            }
            if !errors.is_empty() {
                Err(Error::Errors { span, errors })?
            }
        });
    }
}

impl Visit<Prop> for Analyzer<'_, '_> {
    fn visit(&mut self, n: &Prop) {
        self.computed_prop_mode = ComputedPropMode::Object;

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

impl Visit<TsMethodSignature> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &TsMethodSignature) {
        node.visit_children(self);

        if node.computed {
            self.validate_computed_prop_key(node.span(), &node.key);
        }
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
