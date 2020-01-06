use super::Analyzer;
use crate::{analyzer::ScopeKind, ty::Type};
use swc_common::Spanned;
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    /// TODO: Handle recursive funciton
    fn visit_fn(&mut self, name: Option<&Ident>, f: &Function) -> Type {
        let fn_ty = self.with_child(ScopeKind::Fn, Default::default(), |child| {
            child.return_type_span = f.span();

            let no_implicit_any_span = name.as_ref().map(|name| name.span);

            if let Some(name) = name {
                // We use `typeof function` to infer recursive function's return type.
                match child.scope.declare_var(
                    f.span,
                    VarDeclKind::Var,
                    name.sym.clone(),
                    Some(Type::Simple(Cow::Owned(
                        TsTypeQuery {
                            span: f.span,
                            expr_name: TsEntityName::Ident(name.clone()).into(),
                        }
                        .into(),
                    ))),
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

            match f.type_params {
                Some(TsTypeParamDecl { ref params, .. }) => {
                    params.iter().for_each(|param| {
                        let ty = Type::Param(Param {
                            span: param.span,
                            name: param.name.sym.clone(),
                            constraint: param.constraint.as_ref().map(|v| box v.clone()),
                            default: param.default.as_ref().map(|v| box v.clone()),
                        });

                        child
                            .scope
                            .facts
                            .types
                            .insert(param.name.sym.clone().into(), ty);
                    });
                }
                None => {}
            }

            let old = child.allow_ref_declaring;
            child.allow_ref_declaring = false;

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

            f.params.iter().for_each(|pat| {
                let mut names = vec![];

                let mut visitor = VarVisitor { names: &mut names };

                pat.visit_with(&mut visitor);

                child.declaring.extend_from_slice(&names);

                debug_assert_eq!(child.allow_ref_declaring, false);
                match child.declare_vars(VarDeclKind::Let, pat) {
                    Ok(()) => {}
                    Err(err) => {
                        child.info.errors.push(err);
                    }
                }
                child.scope.remove_declaring(names);
            });

            if let Some(name) = name {
                assert_eq!(child.scope.declaring_fn, None);
                child.scope.declaring_fn = Some(name.sym.clone());
            }

            child.inferred_return_types.get_mut().insert(f.span, vec![]);
            // TODO: Remove clone
            f.clone().fold_children(child);

            let (mut fn_ty, err) = child.type_of_fn_detail(f)?;
            child.info.errors.push(err);
            match fn_ty {
                // Handle tuple widening of the return type.
                Type::Function(ty::Function { ref mut ret_ty, .. }) => {
                    match *ret_ty.normalize_mut() {
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

                                if child.rule.no_implicit_any
                                    && child.span_allowed_implicit_any != f.span
                                {
                                    child.info.errors.push(Error::ImplicitAny {
                                        span: no_implicit_any_span.unwrap_or(span),
                                    });
                                }

                                *t = Type::any(span);
                            }
                        }

                        _ => {}
                    }

                    // Validate return type
                    match child.expand_type(ret_ty.span(), ret_ty) {
                        Ok(ret_ty) => {}
                        Err(err) => child.info.errors.push(err),
                    }
                }

                _ => unreachable!(),
            }

            if let Some(name) = name {
                child.scope.declaring_fn = Some(name.sym.clone());
            }

            debug_assert_eq!(child.allow_ref_declaring, false);
            child.allow_ref_declaring = old;

            Ok(fn_ty)
        });

        match fn_ty {
            Ok(ty) => ty,
            Err(err) => {
                self.info.errors.push(err);
                Type::any(f.span)
            }
        }
    }
}

impl Fold<FnDecl> for Analyzer<'_, '_> {
    /// NOTE: This method **should not call f.fold_children(self)**
    fn fold(&mut self, f: FnDecl) -> FnDecl {
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

        f
    }
}

impl Fold<FnExpr> for Analyzer<'_, '_> {
    /// NOTE: This method **should not call f.fold_children(self)**
    fn fold(&mut self, f: FnExpr) -> FnExpr {
        self.visit_fn(f.ident.as_ref(), &f.function);

        f
    }
}

impl Fold<Function> for Analyzer<'_, '_> {
    fn fold(&mut self, f: Function) -> Function {
        self.visit_fn(None, &f);

        f
    }
}

impl Fold<ArrowExpr> for Analyzer<'_, '_> {
    fn fold(&mut self, f: ArrowExpr) -> ArrowExpr {
        self.with_child(ScopeKind::ArrowFn, Default::default(), |child| {
            child.return_type_span = f.span;

            match f.type_params {
                Some(TsTypeParamDecl { ref params, .. }) => {
                    params.iter().for_each(|param| {
                        let ty = Type::Param(Param {
                            span: param.span,
                            name: param.name.sym.clone(),
                            constraint: param.constraint.as_ref().map(|v| box v.clone()),
                            default: param.default.as_ref().map(|v| box v.clone()),
                        });

                        child
                            .scope
                            .facts
                            .types
                            .insert(param.name.sym.clone().into(), ty);
                    });
                }
                None => {}
            }

            for pat in f.params.iter() {
                match child.declare_vars(VarDeclKind::Let, pat) {
                    Ok(()) => {}
                    Err(err) => {
                        child.info.errors.push(err);
                    }
                }
            }

            let f = f.fold_children(child);

            match f.body {
                BlockStmtOrExpr::Expr(ref expr) => {
                    child.visit_return_arg(expr.span(), Some(expr));
                }
                _ => {}
            }

            f
        })
    }
}
