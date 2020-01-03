use super::{ty::Type, Analyzer};
use crate::{
    errors::Error,
    legacy::{ComputedPropMode, LOG_VISIT},
    ty::TypeRefExt,
};
use swc_atoms::js_word;
use swc_common::{Fold, FoldWith, Spanned};
use swc_ecma_ast::*;

impl Fold<ComputedPropName> for Analyzer<'_, '_> {
    fn fold(&mut self, node: ComputedPropName) -> ComputedPropName {
        // TODO: check if it's class or object literal
        let node = node.fold_children(self);

        let span = node.span;

        let is_symbol_access = match *node.expr {
            Expr::Member(MemberExpr {
                obj:
                    ExprOrSuper::Expr(box Expr::Ident(Ident {
                        sym: js_word!("Symbol"),
                        ..
                    })),
                ..
            }) => true,
            _ => false,
        };

        analyze!(self, {
            let mut errors = vec![];
            let ty = match self.type_of(&node.expr) {
                Ok(ty) => ty,
                Err(err) => {
                    match err {
                        Error::TS2585 { span } => Err(Error::TS2585 { span })?,
                        _ => {}
                    }

                    errors.push(err);
                    // TODO: Change this to something else (maybe any)
                    Type::unknown(span).owned()
                }
            };
            if match self.computed_prop_mode {
                ComputedPropMode::Class { has_body } => !has_body,
                ComputedPropMode::Object => errors.is_empty(),
            } {
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
                    _ if is_symbol_access => {}
                    _ => errors.push(Error::TS2464 { span }),
                }
            }
            if !errors.is_empty() {
                Err(Error::Errors { span, errors })?
            }
        });

        node
    }
}

impl Fold<Prop> for Analyzer<'_, '_> {
    fn fold(&mut self, n: Prop) -> Prop {
        self.computed_prop_mode = ComputedPropMode::Object;

        let n = n.fold_children(self);

        match n {
            Prop::Shorthand(ref i) => {
                analyze!(self, {
                    // TODO: Check if RValue is correct
                    self.type_of_ident(&i, TypeOfMode::RValue)?;
                });
            }
            _ => {}
        }

        n
    }
}

impl Fold<GetterProp> for Analyzer<'_, '_> {
    fn fold(&mut self, n: GetterProp) -> GetterProp {
        let (entry, n) = {
            self.with_child(ScopeKind::Fn, Default::default(), |child| {
                child.return_type_span = n.span();

                child
                    .inferred_return_types
                    .get_mut()
                    .insert(n.span(), Default::default());

                let n = n.fold_children(child);

                (
                    child
                        .inferred_return_types
                        .get_mut()
                        .remove_entry(&n.span())
                        .unwrap_or_default(),
                    n,
                )
            })
        };

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

        n
    }
}

impl Fold<TsMethodSignature> for Analyzer<'_, '_> {
    fn fold(&mut self, node: TsMethodSignature) -> TsMethodSignature {
        if LOG_VISIT {
            println!("Fold<TsMethodSignature>");
        }
        let node = node.fold_children(self);

        if node.computed {
            self.validate_computed_prop_key(node.span(), &node.key);
        }

        node
    }
}

impl Fold<TsPropertySignature> for Analyzer<'_, '_> {
    fn fold(&mut self, node: TsPropertySignature) -> TsPropertySignature {
        let node = node.fold_children(self);

        if node.computed {
            self.validate_computed_prop_key(node.span(), &node.key);
        }

        node
    }
}
