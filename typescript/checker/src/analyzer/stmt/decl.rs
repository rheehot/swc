use super::super::{pat::PatMode, util::PatExt, Analyzer, Ctx};
use crate::{
    analyzer::util::ResultExt,
    errors::Error,
    ty::{Tuple, Type},
    validator::ValidateWith,
};
use swc_common::{Spanned, Visit};
use swc_ecma_ast::*;

impl Visit<VarDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, var: &VarDecl) {
        let kind = var.kind;

        let ctx = Ctx {
            pat_mode: PatMode::Decl,
            in_declare: self.ctx.in_declare || var.declare,
            allow_ref_declaring: true,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|a| {
            var.decls.iter().for_each(|mut v| {
                let res: Result<_, _> = try {
                    let v_span = v.span();

                    let debug_declaring = if cfg!(debug_assertions) {
                        Some(a.scope.declaring.clone())
                    } else {
                        None
                    };

                    macro_rules! inject_any {
                        () => {
                            // Declare variable with type any
                            match a
                                .scope
                                .declare_complex_vars(kind, &v.name, Type::any(v_span))
                            {
                                Ok(()) => {}
                                Err(err) => {
                                    a.info.push_error(err);
                                }
                            }
                        };
                    }

                    macro_rules! remove_declaring {
                        () => {{
                            debug_assert_eq!(Some(a.scope.declaring.clone()), debug_declaring);
                        }};
                    }

                    if v.init.is_some() {
                        a.visit(&v.name);
                    }

                    if let Some(ref init) = v.init {
                        let span = init.span();

                        let declared_ty = v.name.get_ty();
                        if declared_ty.is_some() {
                            //TODO:
                            // self.span_allowed_implicit_any = span;
                        }

                        debug_assert_eq!(a.ctx.allow_ref_declaring, true);

                        //  Check if v_ty is assignable to ty
                        let value_ty = match init.validate_with(a) {
                            Ok(ty) => {
                                let ty = ty;
                                a.check_rvalue(&ty);
                                ty
                            }
                            Err(err) => {
                                a.info.push_error(err);
                                inject_any!();
                                remove_declaring!();
                                return;
                            }
                        };

                        match declared_ty {
                            Some(ty) => {
                                let ty = match ty.validate_with(a) {
                                    Ok(ty) => ty,
                                    Err(err) => {
                                        a.info.push_error(err);
                                        remove_declaring!();
                                        return;
                                    }
                                };
                                let ty = a.expand(span, ty)?;
                                let error = a.assign(&ty, &value_ty, v_span);
                                match error {
                                    Ok(()) => {
                                        match a.scope.declare_complex_vars(kind, &v.name, ty) {
                                            Ok(()) => {}
                                            Err(err) => {
                                                a.info.push_error(err);
                                            }
                                        }
                                        remove_declaring!();
                                        return;
                                    }
                                    Err(err) => {
                                        a.info.push_error(err);
                                        Some(init)
                                    }
                                }
                            }
                            None => {
                                // infer type from value.
                                let mut ty = match value_ty {
                                    Type::EnumVariant(ref v) => {
                                        if let Some(Type::Enum(ref e)) = a.find_type(&v.enum_name) {
                                            Type::Enum(e.clone())
                                        } else {
                                            unreachable!()
                                        }
                                    }
                                    ty => ty,
                                };

                                let mut type_errors = vec![];

                                // Handle implicit any

                                match ty {
                                    Type::Tuple(Tuple { ref mut types, .. }) => {
                                        for (i, t) in types.iter_mut().enumerate() {
                                            let span = t.span();

                                            match *t.normalize() {
                                                Type::Keyword(TsKeywordType {
                                                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                                                    ..
                                                })
                                                | Type::Keyword(TsKeywordType {
                                                    kind: TsKeywordTypeKind::TsNullKeyword,
                                                    ..
                                                }) => {}
                                                _ => {
                                                    continue;
                                                }
                                            }
                                            // Widen tuple types
                                            *t = Type::any(span);

                                            if a.rule.no_implicit_any {
                                                match v.name {
                                                    Pat::Ident(ref i) => {
                                                        let span = i.span;
                                                        type_errors
                                                            .push(Error::ImplicitAny { span });
                                                        break;
                                                    }
                                                    Pat::Array(ArrayPat { ref elems, .. }) => {
                                                        let span = elems[i].span();
                                                        type_errors
                                                            .push(Error::ImplicitAny { span });
                                                    }
                                                    _ => {}
                                                }
                                            }
                                        }
                                    }
                                    _ => {}
                                }

                                if !type_errors.is_empty() {
                                    a.info.push_errors(type_errors);
                                    remove_declaring!();
                                    return;
                                }

                                a.scope
                                    .declare_complex_vars(kind, &v.name, ty)
                                    .store(&mut a.info.errors);
                                remove_declaring!();
                                return;
                            }
                        }
                    } else {
                        match v.name {
                            Pat::Ident(Ident {
                                span,
                                ref sym,
                                ref type_ann,
                                ..
                            }) => {
                                //
                                let sym = sym.clone();
                                let ty = try_opt!(type_ann.validate_with(a));
                                let ty = match ty {
                                    Some(ty) => Some(a.expand(span, ty)?),
                                    None => None,
                                };

                                match a.declare_var(
                                    span,
                                    kind,
                                    sym,
                                    ty,
                                    // initialized
                                    false,
                                    // allow_multiple
                                    kind == VarDeclKind::Var,
                                ) {
                                    Ok(()) => {}
                                    Err(err) => {
                                        a.info.push_error(err);
                                    }
                                };
                            }
                            _ => {
                                // assert!(
                                //     var.declare,
                                //     "complex pattern without initializer is invalid syntax and
                                // parser \      should handle it"
                                //  );

                                if a.ctx.in_declare {
                                    match a.declare_vars(kind, &v.name) {
                                        Ok(()) => {}
                                        Err(err) => {
                                            a.info.push_error(err);
                                        }
                                    };
                                } else {
                                    // This is parsing error
                                }
                            }
                        };
                        remove_declaring!();
                        return;
                    };

                    debug_assert_eq!(a.ctx.allow_ref_declaring, true);
                    a.declare_vars(kind, &v.name).store(&mut a.info.errors);

                    remove_declaring!();
                };

                res.store(&mut a.info.errors);
            });
        });
    }
}
