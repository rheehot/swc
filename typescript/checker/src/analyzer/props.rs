use super::{scope::ScopeKind, Analyzer};
use crate::{
    analyzer::{expr::TypeOfMode, util::ResultExt},
    errors::Error,
    ty::Type,
};
use swc_atoms::js_word;
use swc_common::{Fold, FoldWith, Spanned};
use swc_common::{Spanned, Visit, VisitWith};
use swc_common::{Visit, VisitWith};
use swc_common::{Spanned, Visit, VisitWith};
use swc_common::{Fold, FoldWith, Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

#[derive(Debug, Clone, Copy)]
pub(super) enum ComputedPropMode {
    Class {
        has_body: bool,
    },
    /// Object literal
    Object,
}

impl Visit<ComputedPropName> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &ComputedPropName) {
prevent!(ComputedPropName);

impl Analyzer<'_, '_> {
    #[validator]
    fn visit(&mut self, node: &ComputedPropName) {
        node.visit_children(self);

        let mode = self.computed_prop_mode;

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

        let mut errors = vec![];
        let ty = match self.validate_expr(&node.expr) {
            Ok(ty) => ty,
            Err(err) => {
                match err {
                    Error::TS2585 { span } => Err(Error::TS2585 { span })?,
                    _ => {}
                }

                errors.push(err);
                // TODO: Change this to something else (maybe any)
                Type::unknown(span)
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
    }
}

impl Visit<Prop> for Analyzer<'_, '_> {
    fn visit(&mut self, n: &Prop) {
        self.computed_prop_mode = ComputedPropMode::Object;

        n.visit_children(self);

        match n {
            Prop::Shorthand(ref i) => {
                // TODO: Check if RValue is correct
                self.type_of_ident(&i, TypeOfMode::RValue)
                    .store(&mut self.info.errors);
            }
            _ => {}
        }
    }
}

impl Visit<GetterProp> for Analyzer<'_, '_> {
    fn visit(&mut self, n: &GetterProp) {
        let (entry, n) = {
            self.with_child(ScopeKind::Fn, Default::default(), |child| {
                child.return_type_span = n.span();

                child
                    .inferred_return_types
                    .get_mut()
                    .insert(n.span(), Default::default());

                let n = n.visit_children(child);

                (
                    child
                        .inferred_return_types
                        .get_mut()
                        .remove_entry(&n.span())
                        .unwrap_or_default(),
                    n,
                )
                n.visit_children(child);
            })
        };
impl Visit<GetterProp> for Analyzer<'_> {
    fn visit(&mut self, n: &GetterProp) {
        self.with_child(ScopeKind::Fn, Default::default(), |child| {
            n.key.visit_with(child);

            if let Some(body) = &n.body {
                let ret_ty = child.visit_stmts_for_return(&body.stmts)?;

                if let None = ret_ty {
                    // getter property must have return statements.
                    child
                        .info
                        .errors
                        .push(Error::GetterPropWithoutReturn { span: n.key.span() });
                }
            }

            Ok(())
        })
        .store(&mut self.info.errors);
    }
}

impl Analyzer<'_, '_> {
    fn validate_ts_method_signature(&mut self, node: TsMethodSignature) -> TsMethodSignature {
        let node = node.fold_children(self);
impl Visit<TsMethodSignature> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &TsMethodSignature) {
        node.visit_children(self);
        let node = node.visit_children(self);

        if node.computed {
            self.validate_computed_prop_key(node.span(), &node.key);
        }
    }
}

    fn validate_ts_property_signature(
        &mut self,
        node: TsPropertySignature,
    ) -> Result<TsPropertySignature, Error> {
        let node = node.fold_children(self);
impl Visit<TsPropertySignature> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &TsPropertySignature) {
        node.visit_children(self);
        let node = node.visit_children(self);

        if node.computed {
            self.validate_computed_prop_key(node.span(), &node.key);
        }
    }
}
