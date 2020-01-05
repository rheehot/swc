use super::Analyzer;
use crate::{
    analyzer::util::ResultExt,
    builtin_types,
    errors::Error,
    ty,
    ty::{ClassInstance, Tuple, Type, TypeLit, TypeParamInstantiation},
    util::RemoveTypes,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, VisitWith};
use swc_ecma_ast::*;

mod bin;
mod call_new;
mod type_cast;
mod unary;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeOfMode {
    /// Used for l-values.
    ///
    /// This is used to allow
    ///
    /// ```ts
    /// type Num = { '0': string } | { [n: number]: number }
    /// declare var num: Num
    /// num[0] = 1
    /// num['0'] = 'ok'
    /// ```
    LValue,
    /// Use for r-values.
    RValue,
}

prevent!(Expr);

impl Validate<Expr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &Expr) -> Self::Output {
        self.validate_expr(e, TypeOfMode::RValue, None)
    }
}

prevent!(ParenExpr);

impl Validate<ParenExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &ParenExpr) -> Self::Output {
        self.validate(&e.expr)
    }
}

prevent!(AssignExpr);

impl Validate<AssignExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &AssignExpr) -> Self::Output {
        match e.left {
            PatOrExpr::Pat(box Pat::Ident(ref i)) | PatOrExpr::Expr(box Expr::Ident(ref i)) => {
                // Type is any if self.declaring contains ident
            }
        }

        let mut errors = vec![];
        let span = e.span();
        e.visit_children(self);

        let rhs_ty = match self.validate(&e.right) {
            Ok(rhs_ty) => {
                let rhs_ty = rhs_ty;

                self.check_rvalue(&rhs_ty);

                Ok(rhs_ty)
            }
            Err(err) => {
                errors.push(err);
                Err(())
            }
        };

        self.info.errors.extend(errors);

        let rhs_ty = match rhs_ty {
            Ok(v) => v,
            Err(()) => Type::any(span),
        };

        if e.op == op!("=") {
            self.try_assign(span, &e.left, &rhs_ty);
        }

        Ok(rhs_ty)
    }
}

prevent!(UpdateExpr);

impl Validate<UpdateExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &UpdateExpr) -> Self::Output {
        let span = e.span;

        let res = self
            .validate_expr(&e.arg, TypeOfMode::LValue, None)
            .and_then(|ty| match *ty.normalize() {
                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Lit(TsLitType {
                    lit: TsLit::Str(..),
                    ..
                })
                | Type::Array(..) => Err(Error::TS2356 { span: e.arg.span() }),

                _ => Ok(()),
            })
            .store(&mut self.info.errors);

        Ok(Type::Keyword(TsKeywordType {
            kind: TsKeywordTypeKind::TsNumberKeyword,
            span,
        }))
    }
}

prevent!(SeqExpr);

impl Validate<SeqExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &SeqExpr) -> Self::Output {
        let SeqExpr { span, ref exprs } = *e;

        assert!(exprs.len() >= 1);

        let mut is_any = false;
        for e in exprs.iter() {
            match **e {
                Expr::Ident(ref i) => {
                    if self.scope.declaring.contains(&i.sym) {
                        is_any = true;
                    }
                }
                _ => {}
            }
            match self.validate(e) {
                Ok(..) => {}
                Err(Error::ReferencedInInit { .. }) => {
                    is_any = true;
                }
                Err(..) => {}
            }
        }
        if is_any {
            return Ok(Type::any(span));
        }

        return self.validate(&exprs.last().unwrap());
    }
}

impl Analyzer<'_, '_> {
    pub fn validate_expr(
        &mut self,
        e: &Expr,
        mode: TypeOfMode,
        type_args: Option<TypeParamInstantiation>,
    ) -> ValidationResult<Type> {
        let span = e.span();

        match e {
            // super() returns any
            Expr::Call(CallExpr {
                callee: ExprOrSuper::Super(..),
                ..
            }) => Ok(Type::any(span)),

            Expr::Bin(e) => self.validate(e),
            Expr::Update(e) => self.validate(e),
            Expr::New(e) => self.validate(e),
            Expr::Call(e) => self.validate(e),
            Expr::TsAs(e) => self.validate(e),
            Expr::TsTypeAssertion(e) => self.validate(e),
            Expr::Assign(e) => e.validate_with(self),

            Expr::This(ThisExpr { span }) => {
                let span = *span;
                if let Some(ty) = self.scope.this() {
                    return Ok(ty.clone());
                }
                return Ok(Type::from(TsThisType { span }));
            }

            Expr::Ident(ref i) => self.type_of_ident(i, mode),

            Expr::Array(ArrayLit { ref elems, .. }) => {
                let mut types: Vec<Type> = Vec::with_capacity(elems.len());

                for elem in elems {
                    let span = elem.span();
                    match elem {
                        Some(ExprOrSpread {
                            spread: None,
                            ref expr,
                        }) => {
                            let ty = self.validate(expr)?;
                            types.push(ty)
                        }
                        Some(ExprOrSpread {
                            spread: Some(..), ..
                        }) => unimplemented!("type of array spread"),
                        None => {
                            let ty = Type::undefined(span);
                            types.push(ty)
                        }
                    }
                }

                return Ok(Type::Tuple(Tuple { span, types }));
            }

            Expr::Lit(Lit::Bool(v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Bool(*v),
                }));
            }
            Expr::Lit(Lit::Str(ref v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Str(v.clone()),
                }));
            }
            Expr::Lit(Lit::Num(v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Number(*v),
                }));
            }
            Expr::Lit(Lit::Null(Null { span })) => {
                return Ok(Type::Keyword(TsKeywordType {
                    span: *span,
                    kind: TsKeywordTypeKind::TsNullKeyword,
                }));
            }
            Expr::Lit(Lit::Regex(..)) => {
                return Ok(Type::TsTypeRef(TsTypeRef {
                    span,
                    type_name: TsEntityName::Ident(Ident {
                        span,
                        sym: js_word!("RegExp"),
                        optional: false,
                        type_ann: None,
                    }),
                    type_params: None,
                }));
            }

            Expr::Paren(ParenExpr { ref expr, .. }) => self.validate(&expr),

            Expr::Tpl(ref t) => {
                // Check if tpl is constant. If it is, it's type is string literal.
                if t.exprs.is_empty() {
                    return Ok(Type::Lit(TsLitType {
                        span: t.span(),
                        lit: TsLit::Str(
                            t.quasis[0]
                                .cooked
                                .clone()
                                .unwrap_or_else(|| t.quasis[0].raw.clone()),
                        ),
                    }));
                }

                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                }));
            }

            Expr::TsNonNull(TsNonNullExpr { ref expr, .. }) => {
                Ok(expr.validate_with(self)?.remove_falsy())
            }

            Expr::Object(ObjectLit { span, ref props }) => {
                let mut members = Vec::with_capacity(props.len());
                let mut special_type = None;

                for prop in props.iter() {
                    match *prop {
                        PropOrSpread::Prop(ref prop) => {
                            members.push(props.validate_with(self)?);
                        }
                        PropOrSpread::Spread(SpreadElement { ref expr, .. }) => {
                            match self.validate(&expr)? {
                                Type::TypeLit(TypeLit {
                                    members: spread_members,
                                    ..
                                }) => {
                                    members.extend(spread_members);
                                }

                                // Use last type on ...any or ...unknown
                                ty
                                @
                                Type::Keyword(TsKeywordType {
                                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                                    ..
                                })
                                | ty
                                @
                                Type::Keyword(TsKeywordType {
                                    kind: TsKeywordTypeKind::TsAnyKeyword,
                                    ..
                                }) => special_type = Some(ty),

                                ty => unimplemented!("spread with non-type-lit: {:#?}", ty),
                            }
                        }
                    }
                }

                if let Some(ty) = special_type {
                    return Ok(ty);
                }

                return Ok(Type::TypeLit(TypeLit {
                    span: *span,
                    members,
                }));
            }

            // https://github.com/Microsoft/TypeScript/issues/26959
            Expr::Yield(..) => return Ok(Type::any(span)),

            Expr::Update(..) => {
                return Ok(Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    span,
                }));
            }

            Expr::Await(AwaitExpr { .. }) => unimplemented!("typeof(AwaitExpr)"),

            Expr::Class(ClassExpr { ref class, .. }) => {
                return Ok(self.type_of_class(None, class)?);
            }

            Expr::Arrow(ref e) => return Ok(self.type_of_arrow_fn(e)?),

            Expr::Fn(FnExpr { ref function, .. }) => {
                return Ok(self.type_of_fn(&function)?);
            }

            Expr::Member(ref expr) => {
                return self.type_of_member_expr(expr, mode);
            }

            Expr::MetaProp(..) => unimplemented!("typeof(MetaProp)"),

            Expr::Invalid(ref i) => return Ok(Type::any(i.span())),

            _ => unimplemented!("typeof ({:#?})", e),
        }
    }

    pub fn type_of_ident(&mut self, i: &Ident, type_mode: TypeOfMode) -> ValidationResult {
        let span = i.span();

        match i.sym {
            js_word!("arguments") => return Ok(Type::any(span)),
            js_word!("Symbol") => {
                return Ok(builtin_types::get_var(
                    self.libs,
                    i.span,
                    &js_word!("Symbol"),
                )?);
            }
            js_word!("undefined") => return Ok(Type::undefined(span)),
            js_word!("void") => return Ok(Type::any(span)),
            js_word!("eval") => match type_mode {
                TypeOfMode::LValue => return Err(Error::CannotAssignToNonVariable { span }),
                TypeOfMode::RValue => {
                    return Ok(Type::Function(ty::Function {
                        span,
                        params: vec![],
                        ret_ty: box Type::any(span),
                        type_params: None,
                    }));
                }
            },
            _ => {}
        }

        if i.sym == js_word!("require") {
            unreachable!("typeof(require('...'))");
        }

        if let Some(ty) = self.resolved_imports.get(&i.sym) {
            println!(
                "({}) type_of({}): resolved import",
                self.scope.depth(),
                i.sym
            );
            return Ok(Type::Arc(ty.clone()));
        }

        if let Some(ty) = self.find_type(&i.sym) {
            println!("({}) type_of({}): find_type", self.scope.depth(), i.sym);
            return Ok(ty.clone().respan(span));
        }

        // Check `declaring` before checking variables.
        if self.scope.declaring.contains(&i.sym) {
            println!(
                "({}) reference in initialization: {}",
                self.scope.depth(),
                i.sym
            );

            if self.allow_ref_declaring {
                return Ok(Type::any(span));
            } else {
                return Err(Error::ReferencedInInit { span });
            }
        }

        if let Some(ty) = self.find_var_type(&i.sym) {
            println!("({}) type_of({}): find_var_type", self.scope.depth(), i.sym);
            return Ok(ty.clone().respan(span));
        }

        if let Some(_var) = self.find_var(&i.sym) {
            // TODO: Infer type or use type hint to handle
            //
            // let id: (x: Foo) => Foo = x => x;
            //
            return Ok(Type::any(span));
        }

        if let Ok(ty) = builtin_types::get_var(self.libs, span, &i.sym) {
            return Ok(ty);
        }

        println!(
            "({}) type_of(): undefined symbol: {}",
            self.scope.depth(),
            i.sym,
        );

        return Err(Error::UndefinedSymbol { span: i.span });
    }

    pub fn type_of_ts_entity_name(
        &self,
        span: Span,
        n: &TsEntityName,
        type_args: Option<TypeParamInstantiation>,
    ) -> ValidationResult {
        match *n {
            TsEntityName::Ident(ref i) => self.type_of_ident(i, TypeOfMode::RValue),
            TsEntityName::TsQualifiedName(ref qname) => {
                let obj_ty = self.type_of_ts_entity_name(span, &qname.left, None)?;

                self.access_property(
                    span,
                    obj_ty,
                    &Expr::Ident(qname.right.clone()),
                    false,
                    TypeOfMode::RValue,
                )
            }
        }
    }
}

fn instantiate_class(ty: Type) -> Type {
    let span = ty.span();

    match *ty.normalize() {
        Type::Tuple(Tuple { ref types, span }) => Type::Tuple(Tuple {
            span,
            types: types
                .iter()
                .map(|ty| {
                    // TODO: Remove clone
                    instantiate_class(ty.clone())
                })
                .collect(),
        }),
        Type::Class(ref cls) => Type::ClassInstance(ClassInstance {
            // TODO
            span,

            // TODO; Remove clone
            cls: cls.clone(),

            // TODO
            type_args: None,
        }),
        _ => ty,
    }
}
