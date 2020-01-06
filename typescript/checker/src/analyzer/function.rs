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
        let fn_ty: Result<_, _> = try {
            let no_implicit_any_span = name.as_ref().map(|name| name.span);

            if let Some(name) = name {
                // We use `typeof function` to infer recursive function's return type.
                match self.scope.declare_var(
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
                        self.info.errors.push(err);
                    }
                }
            }

            if let Some(name) = name {
                assert_eq!(self.scope.declaring_fn, None);
                self.scope.declaring_fn = Some(name.sym.clone());
            }

            let mut fn_ty = f.validate_with(self)?;
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
                self.scope.declaring_fn = Some(name.sym.clone());
            }

            fn_ty
        };

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
