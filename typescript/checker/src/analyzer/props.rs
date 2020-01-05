use super::{scope::ScopeKind, Analyzer};
use crate::{
    analyzer::{expr::TypeOfMode, util::ResultExt},
    errors::Error,
    ty::{MethodSignature, PropertySignature, Type, TypeElement},
    validator::{Validate, ValidateWith},
    ValidationResult,
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
        let ty = match self.validate(&node.expr) {
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

impl Validate<Prop> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeElement>;

    fn validate(&mut self, prop: &Prop) -> Self::Output {
        self.computed_prop_mode = ComputedPropMode::Object;

        prop.visit_children(self);

        match prop {
            Prop::Shorthand(ref i) => {
                // TODO: Check if RValue is correct
                self.type_of_ident(&i, TypeOfMode::RValue)
                    .store(&mut self.info.errors);
            }
            _ => {}
        }

        let span = prop.span();

        Ok(match *prop {
            Prop::Shorthand(..) => PropertySignature {
                span: prop.span(),
                key: prop_key_to_expr(&prop),
                params: Default::default(),
                optional: false,
                readonly: false,
                computed: false,
                type_ann: Default::default(),
                type_params: Default::default(),
            }
            .into(),

            Prop::KeyValue(ref kv) => {
                let ty = kv.value.validate_with(self)?;

                PropertySignature {
                    span: prop.span(),
                    key: prop_key_to_expr(&prop),
                    params: Default::default(),
                    optional: false,
                    readonly: false,
                    computed: false,
                    type_ann: Some(ty),
                    type_params: Default::default(),
                }
                .into()
            }

            Prop::Assign(ref p) => unimplemented!("type_of_prop(AssignProperty): {:?}", p),
            Prop::Getter(ref p) => PropertySignature {
                span: prop.span(),
                key: prop_key_to_expr(&prop),
                params: Default::default(),
                optional: false,
                readonly: false,
                computed: false,
                type_ann: match p
                    .body
                    .as_ref()
                    .map(|bs| self.visit_stmts_for_return(&bs.stmts))
                {
                    Some(ty) => ty?,
                    // This is error, but it's handled by GetterProp visitor.
                    None => None,
                },
                type_params: Default::default(),
            }
            .into(),
            Prop::Setter(ref p) => unimplemented!("type_of_prop(SetterProperty): {:?}", p),

            Prop::Method(ref p) => MethodSignature {
                span,
                readonly: false,
                key: prop_key_to_expr(&prop),
                computed: false,
                optional: false,
                params: p.function.params.validate_with(self)?,
                ret_ty: try_opt!(p.function.return_type.validate_with(self)),
                type_params: try_opt!(p.function.type_params.validate_with(self)),
            }
            .into(),
        })
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

fn prop_key_to_expr(p: &Prop) -> Box<Expr> {
    match *p {
        Prop::Shorthand(ref i) => box Expr::Ident(i.clone()),
        Prop::Assign(AssignProp { ref key, .. }) => box Expr::Ident(key.clone()),
        Prop::Getter(GetterProp { ref key, .. })
        | Prop::KeyValue(KeyValueProp { ref key, .. })
        | Prop::Method(MethodProp { ref key, .. })
        | Prop::Setter(SetterProp { ref key, .. }) => prop_name_to_expr(key),
    }
}

pub(super) fn prop_name_to_expr(key: &PropName) -> Box<Expr> {
    match *key {
        PropName::Computed(ref p) => p.expr.clone(),
        PropName::Ident(ref ident) => box Expr::Ident(ident.clone()),
        PropName::Str(ref s) => box Expr::Lit(Lit::Str(Str { ..s.clone() })),
        PropName::Num(ref s) => box Expr::Lit(Lit::Num(Number { ..s.clone() })),
    }
}
