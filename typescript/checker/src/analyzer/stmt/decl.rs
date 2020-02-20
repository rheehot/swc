use super::super::{pat::PatMode, Analyzer, Ctx};
use crate::{
    analyzer::util::ResultExt,
    errors::Error,
    ty::{Tuple, Type},
    util::PatExt,
    validator::{Validate, ValidateWith},
};
use swc_common::Spanned;
use swc_ecma_ast::*;

#[validator]
impl Validate<VarDecl> for Analyzer<'_, '_> {
    type Output = ();

    fn validate(&mut self, var: &mut VarDecl) {
        self.record(&*var);

        let kind = var.kind;

        let ctx = Ctx {
            pat_mode: PatMode::Decl,
            in_declare: self.ctx.in_declare || var.declare,
            allow_ref_declaring: true,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|a| {
            var.decls.iter_mut().for_each(|v| {
                a.record(v);

                let res: Result<_, _> = try {
                    let v_span = v.span();
                    if !a.is_builtin {
                        debug_assert!(!v_span.is_dummy());
                    }

                    let debug_declaring = if cfg!(debug_assertions) {
                        Some(a.scope.declaring.clone())
                    } else {
                        None
                    };

                    macro_rules! inject_any {
                        () => {
                            // Declare variable with type any
                            match a.declare_complex_vars(kind, &v.name, Type::any(v_span)) {
                                Ok(()) => {}
                                Err(err) => {
                                    a.info.errors.push(err);
                                }
                            }
                        };
                    }

                    macro_rules! remove_declaring {
                        () => {{
                            debug_assert_eq!(Some(a.scope.declaring.clone()), debug_declaring);
                        }};
                    }

                    if let Some(ref mut init) = v.init {
                        let span = init.span();

                        let declared_ty = v.name.get_mut_ty();
                        if declared_ty.is_some() {
                            //TODO:
                            // self.span_allowed_implicit_any = span;
                        }

                        debug_assert_eq!(a.ctx.allow_ref_declaring, true);

                        //  Check if v_ty is assignable to ty
                        let value_ty = match init.validate_with(a) {
                            Ok(ty) => {
                                let ty = a.expand(span, ty)?;
                                a.check_rvalue(&ty);
                                ty
                            }
                            Err(err) => {
                                if a.is_builtin {
                                    unreachable!("failed to assign builtin: \nError: {:?}", err)
                                } else {
                                    a.info.errors.push(err);
                                }
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
                                        a.info.errors.push(err);
                                        remove_declaring!();
                                        return;
                                    }
                                };
                                let ty = a.expand(span, ty)?;
                                match a.assign(&ty, &value_ty, v_span) {
                                    Ok(()) => {
                                        match a.declare_complex_vars(kind, &v.name, ty) {
                                            Ok(()) => {}
                                            Err(err) => {
                                                a.info.errors.push(err);
                                            }
                                        }
                                        remove_declaring!();
                                        return;
                                    }
                                    Err(err) => {
                                        a.info.errors.push(err);
                                        Some(init)
                                    }
                                }
                            }
                            None => {
                                // infer type from value.
                                let mut ty = (|| {
                                    match value_ty {
                                        Type::EnumVariant(ref v) => {
                                            if let Some(items) = a.find_type(&v.enum_name) {
                                                for ty in items {
                                                    if let Type::Enum(ref e) = ty {
                                                        return Type::Enum(e.clone());
                                                    }
                                                }
                                            }
                                        }
                                        _ => {}
                                    }

                                    value_ty
                                })();

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
                                    a.info.errors.extend(type_errors);
                                    remove_declaring!();
                                    return;
                                }

                                a.declare_complex_vars(kind, &v.name, ty)
                                    .store(&mut a.info.errors);
                                remove_declaring!();
                                return;
                            }
                        }
                    } else {
                        match v.name {
                            Pat::Ident(Ident {
                                span,
                                ref mut sym,
                                ref mut type_ann,
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
                                        a.info.errors.push(err);
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
                                    match a.declare_vars(kind, &mut v.name) {
                                        Ok(()) => {}
                                        Err(err) => {
                                            a.info.errors.push(err);
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
                    a.declare_vars(kind, &mut v.name).store(&mut a.info.errors);

                    remove_declaring!();
                };

                res.store(&mut a.info.errors);
            });
        });
    }
}
