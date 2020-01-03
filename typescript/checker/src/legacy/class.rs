use super::Analyzer;
use crate::{
    errors::Error,
    legacy::{ComputedPropMode, VarVisitor, LOG_VISIT},
    ty::{self, Type},
};
use std::mem;
use swc_atoms::js_word;
use swc_common::{util::move_map::MoveMap, Fold, FoldWith, Span, Spanned, VisitWith, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ts_checker_macros::validator;

/// # Validations
///
///  - TS2515: Validate that class implements all methods.
impl Fold<Class> for Analyzer<'_, '_> {
    fn fold(&mut self, c: Class) -> Class {
        log_fold!(c);

        let c = c.fold_children(self);

        self.resolve_parent_interfaces(&c.implements);

        let mut constructor_spans = vec![];
        let mut constructor_required_param_count = None;

        for m in &c.body {
            match *m {
                ClassMember::Constructor(ref cons) => {
                    //
                    if cons.body.is_none() {
                        for p in &cons.params {
                            match *p {
                                PatOrTsParamProp::TsParamProp(..) => {
                                    self.info.errors.push(Error::TS2369 { span: p.span() })
                                }
                                _ => {}
                            }
                        }
                    }

                    {
                        // Check parameter count
                        let required_param_count = cons
                            .params
                            .iter()
                            .filter(|p| match p {
                                PatOrTsParamProp::Pat(Pat::Ident(Ident {
                                    optional: true, ..
                                })) => false,
                                _ => true,
                            })
                            .count();

                        match constructor_required_param_count {
                            Some(v) if required_param_count != v => {
                                for span in constructor_spans.drain(..) {
                                    self.info.errors.push(Error::TS2394 { span })
                                }
                            }

                            None => constructor_required_param_count = Some(required_param_count),
                            _ => {}
                        }
                    }

                    constructor_spans.push(cons.span);
                }

                _ => {}
            }
        }

        c
    }
}

impl Fold<ClassExpr> for Analyzer<'_, '_> {
    fn fold(&mut self, c: ClassExpr) -> ClassExpr {
        log_fold!(c);

        let ty = match self.validate_type_of_class(c.ident.clone().map(|v| v.sym), &c.class) {
            Ok(ty) => ty,
            Err(err) => {
                self.info.errors.push(err);
                Type::any(c.span()).into()
            }
        };

        let old_this = self.scope.this.take();
        self.scope.this = Some(ty.clone());

        let c = self.with_child(ScopeKind::Block, Default::default(), |analyzer| {
            if let Some(ref i) = c.ident {
                analyzer.scope.register_type(i.sym.clone(), ty.clone());

                analyzer.validate_inherited_members(None, &c.class, false);
                analyzer.validate_class_members(&c.class, false);

                match analyzer.scope.declare_var(
                    ty.span(),
                    VarDeclKind::Var,
                    i.sym.clone(),
                    Some(ty),
                    // initialized = true
                    true,
                    // declare Class does not allow multiple declarations.
                    false,
                ) {
                    Ok(()) => {}
                    Err(err) => {
                        analyzer.info.errors.push(err);
                    }
                }
            }

            c.fold_children(analyzer)
        });

        self.scope.this = old_this;

        c
    }
}

impl Fold<ClassDecl> for Analyzer<'_, '_> {
    fn fold(&mut self, c: ClassDecl) -> ClassDecl {
        log_fold!(c);

        let c: ClassDecl = c.fold_children(self);

        self.validate_inherited_members(Some(&c.ident), &c.class, c.declare);
        self.validate_class_members(&c.class, c.declare);

        let ty = match self.validate_type_of_class(Some(c.ident.sym.clone()), &c.class) {
            Ok(ty) => ty,
            Err(err) => {
                self.info.errors.push(err);
                Type::any(c.span()).into()
            }
        };

        let old_this = self.scope.this.take();
        self.scope.this = Some(ty.clone());

        self.scope.register_type(c.ident.sym.clone(), ty.clone());

        match self.scope.declare_var(
            ty.span(),
            VarDeclKind::Var,
            c.ident.sym.clone(),
            Some(ty),
            // initialized = true
            true,
            // declare Class does not allow multiple declarations.
            false,
        ) {
            Ok(()) => {}
            Err(err) => {
                self.info.errors.push(err);
            }
        }

        self.scope.this = old_this;

        c
    }
}
