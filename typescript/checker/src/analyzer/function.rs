use super::Analyzer;
use crate::{
    analyzer::{pat::PatMode, Ctx, ScopeKind},
    errors::Error,
    ty,
    ty::{Param, QueryType, Tuple, Type, TypeParamDecl},
    validator::ValidateWith,
    ValidationResult,
};
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    /// TODO: Handle recursive funciton
    fn visit_fn(&mut self, name: Option<&Ident>, f: &Function) -> Type {
        let fn_ty = self.with_child(ScopeKind::Fn, Default::default(), |child| {
            let no_implicit_any_span = name.as_ref().map(|name| name.span);

            if let Some(name) = name {
                // We use `typeof function` to infer recursive function's return type.
                match child.scope.declare_var(
                    f.span,
                    VarDeclKind::Var,
                    name.sym.clone(),
                    Some(Type::Query(QueryType {
                        span: f.span,
                        expr: TsEntityName::Ident(name.clone()).into(),
                    })),
                    // value is initialized
                    true,
                    // Allow overriding
                    true,
                ) {
                    Ok(()) => {}
                    Err(err) => {
                        child.info.errors.push(err);
                    }
                }
            }

            let type_params: Option<TypeParamDecl> = try_opt!(f.type_params.validate_with(child));
            if let Some(type_params) = type_params {
                type_params.params.into_iter().for_each(|param| {
                    let name = param.name.clone();
                    let ty = Type::Param(Param {
                        span: param.span,
                        name: param.name,
                        constraint: param.constraint,
                        default: param.default,
                    });

                    child.scope.facts.types.insert(name.into(), ty);
                });
            }

            {
                // Validate params
                // TODO: Move this to parser
                let mut has_optional = false;
                for p in &f.params {
                    if has_optional {
                        child.info.errors.push(Error::TS1016 { span: p.span() });
                    }

                    match *p {
                        Pat::Ident(Ident { optional, .. }) => {
                            if optional {
                                has_optional = true;
                            }
                        }
                        _ => {}
                    }
                }
            }

            let ctx = Ctx {
                pat_mode: PatMode::Decl,
                allow_ref_declaring: false,
                ..child.ctx
            };
            child.with_ctx(ctx).visit(&f.params);

            if let Some(name) = name {
                assert_eq!(child.scope.declaring_fn, None);
                child.scope.declaring_fn = Some(name.sym.clone());
            }

            f.visit_children(child);

            let mut fn_ty = f.validate_with(child)?;
            match fn_ty {
                // Handle tuple widening of the return type.
                ty::Function { ref mut ret_ty, .. } => {
                    match **ret_ty {
                        Type::Tuple(Tuple { ref mut types, .. }) => {
                            for t in types.iter_mut() {
                                let span = t.span();

                                match t.normalize() {
                                    Type::Keyword(TsKeywordType {
                                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                        ..
                                    })
                                    | Type::Keyword(TsKeywordType {
                                        kind: TsKeywordTypeKind::TsNullKeyword,
                                        ..
                                    }) => {}
                                    _ => continue,
                                }

                                //if child.rule.no_implicit_any
                                //    && child.span_allowed_implicit_any != f.span
                                //{
                                //    child.info.errors.push(Error::ImplicitAny {
                                //        span: no_implicit_any_span.unwrap_or(span),
                                //    });
                                //}

                                *t = Type::any(span);
                            }
                        }

                        _ => {}
                    }
                }
            }

            if let Some(name) = name {
                child.scope.declaring_fn = Some(name.sym.clone());
            }

            Ok(fn_ty)
        });

        match fn_ty {
            Ok(ty) => ty.into(),
            Err(err) => {
                self.info.errors.push(err);
                Type::any(f.span)
            }
        }
    }
}

impl Visit<FnDecl> for Analyzer<'_, '_> {
    /// NOTE: This method **should not call f.fold_children(self)**
    fn visit(&mut self, f: &FnDecl) {
        let fn_ty = self.visit_fn(Some(&f.ident), &f.function);

        match self
            .scope
            .override_var(VarDeclKind::Var, f.ident.sym.clone(), fn_ty)
        {
            Ok(()) => {}
            Err(err) => {
                self.info.errors.push(err);
            }
        }
    }
}

impl Visit<FnExpr> for Analyzer<'_, '_> {
    /// NOTE: This method **should not call f.fold_children(self)**
    fn visit(&mut self, f: &FnExpr) {
        self.visit_fn(f.ident.as_ref(), &f.function);
    }
}

impl Visit<Function> for Analyzer<'_, '_> {
    fn visit(&mut self, f: &Function) {
        self.visit_fn(None, &f);
    }
}
