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
        let ctx = Ctx {
            pat_mode: PatMode::Decl,
            in_declare: self.ctx.in_declare || var.declare,
            ..self.ctx
        };
        self.with_ctx(ctx)
            .validate_var_declarators(var.kind, &var.decls);
    }
}

impl Analyzer<'_, '_> {
    fn validate_var_declarators(&mut self, kind: VarDeclKind, decls: &[VarDeclarator]) {
        decls.iter().for_each(|mut v| {
            let res: Result<_, _> = try {
                let v_span = v.span();

                let debug_declaring = if cfg!(debug_assertions) {
                    Some(self.scope.declaring.clone())
                } else {
                    None
                };
                let mut names = vec![];

                macro_rules! inject_any {
                    () => {
                        // Declare variable with type any
                        match self
                            .scope
                            .declare_complex_vars(kind, &v.name, Type::any(v_span))
                        {
                            Ok(()) => {}
                            Err(err) => {
                                self.info.errors.push(err);
                            }
                        }
                    };
                }

                macro_rules! remove_declaring {
                    () => {{
                        self.scope.remove_declaring(names);
                        debug_assert_eq!(Some(self.scope.declaring.clone()), debug_declaring);
                    }};
                }

                if v.init.is_some() {
                    let ctx = Ctx {
                        allow_ref_declaring: true,
                        ..self.ctx
                    };
                    self.with_ctx(ctx).visit(&v.name);
                }

                if let Some(init) = v.init {
                    let span = init.span();

                    let declared_ty = v.name.get_ty();
                    if declared_ty.is_some() {
                        //TODO:
                        // self.span_allowed_implicit_any = span;
                    }

                    debug_assert_eq!(self.ctx.allow_ref_declaring, true);

                    //  Check if v_ty is assignable to ty
                    let value_ty = match init.validate_with(self) {
                        Ok(ty) => {
                            let ty = ty;
                            self.check_rvalue(&ty);
                            ty
                        }
                        Err(err) => {
                            self.info.errors.push(err);
                            inject_any!();
                            remove_declaring!();
                            return;
                        }
                    };

                    match declared_ty {
                        Some(ty) => {
                            let ty = match ty.validate_with(self) {
                                Ok(ty) => ty,
                                Err(err) => {
                                    self.info.errors.push(err);
                                    remove_declaring!();
                                    return;
                                }
                            };
                            let error = self.assign(&ty, &value_ty, v_span);
                            match error {
                                Ok(()) => {
                                    match self.scope.declare_complex_vars(kind, &v.name, ty) {
                                        Ok(()) => {}
                                        Err(err) => {
                                            self.info.errors.push(err);
                                        }
                                    }
                                    remove_declaring!();
                                    return;
                                }
                                Err(err) => {
                                    self.info.errors.push(err);
                                    Some(init)
                                }
                            }
                        }
                        None => {
                            // infer type from value.
                            let mut ty = match value_ty {
                                Type::EnumVariant(ref v) => {
                                    if let Some(Type::Enum(ref e)) = self.find_type(&v.enum_name) {
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

                                        if self.rule.no_implicit_any {
                                            match v.name {
                                                Pat::Ident(ref i) => {
                                                    let span = i.span;
                                                    type_errors.push(Error::ImplicitAny { span });
                                                    break;
                                                }
                                                Pat::Array(ArrayPat { ref elems, .. }) => {
                                                    let span = elems[i].span();
                                                    type_errors.push(Error::ImplicitAny { span });
                                                }
                                                _ => {}
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }

                            if !type_errors.is_empty() {
                                self.info.errors.extend(type_errors);
                                remove_declaring!();
                                return;
                            }

                            self.scope
                                .declare_complex_vars(kind, &v.name, ty)
                                .store(&mut self.info.errors);
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
                            let ty = try_opt!(type_ann.validate_with(self));

                            match self.scope.declare_var(
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
                                    self.info.errors.push(err);
                                }
                            };
                        }
                        _ => {
                            // assert!(
                            //     var.declare,
                            //     "complex pattern without initializer is invalid syntax and parser
                            // \      should handle it"
                            //  );

                            if self.ctx.in_declare {
                                match self.declare_vars(kind, &v.name) {
                                    Ok(()) => {}
                                    Err(err) => {
                                        self.info.errors.push(err);
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

                debug_assert_eq!(self.ctx.allow_ref_declaring, true);
                self.declare_vars(kind, &v.name)
                    .store(&mut self.info.errors);

                remove_declaring!();
            };

            res.store(&mut self.info.errors);
        });
    }
}
