use super::Analyzer;
use crate::{
    analyzer::util::ResultExt,
    builtin_types,
    errors::Error,
    ty,
    ty::{ClassInstance, Tuple, Type, TypeLit, TypeParamInstantiation},
    util::RemoveTypes,
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
pub(super) enum TypeOfMode {
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
prevent!(ParenExpr);

impl Analyzer<'_, '_> {
    pub(super) fn validate_expr(&mut self, e: &Expr) -> ValidationResult {
        self.validate_expr_with_extra(e, TypeOfMode::RValue, None)
    }

    pub(super) fn validate_expr_with_extra(
        &mut self,
        e: &Expr,
        mode: TypeOfMode,
        type_args: Option<TypeParamInstantiation>,
    ) -> Result<Type, Error> {
        let span = e.span();

        match e {
            // super() returns any
            Expr::Call(CallExpr {
                callee: ExprOrSuper::Super(..),
                ..
            }) => Ok(Type::any(span)),

            Expr::Bin(e) => self.validate_bin_expr(e),
            Expr::Update(e) => self.validate_update_expr(e),
            Expr::New(e) => self.validate_new_expr(e),
            Expr::Call(e) => self.validate_call_expr(e),
            Expr::TsAs(e) => self.validate_ts_as_expr(e),
            Expr::TsTypeAssertion(e) => self.validate_ts_type_assertion(e),
            //
            Expr::This(ThisExpr { span }) => {
                if let Some(ref ty) = self.scope.this() {
                    return Ok(ty.static_cast());
                }
                return Ok(Type::from(TsThisType { span }).owned());
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
                            let ty = self.validate_expr(expr)?;
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
                    lit: TsLit::Bool(v),
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
                    lit: TsLit::Number(v),
                }));
            }
            Expr::Lit(Lit::Null(Null { span })) => {
                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNullKeyword,
                }));
            }
            Expr::Lit(Lit::Regex(..)) => {
                return Ok(TsType::TsTypeRef(TsTypeRef {
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

            Expr::Paren(ParenExpr { ref expr, .. }) => self.validate_expr(&expr),

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

            Expr::Bin(ref expr) => self.type_of_bin_expr(&expr),

            Expr::TsNonNull(TsNonNullExpr { ref expr, .. }) => {
                let expr = self.validate_expr(&expr)?;

                Ok(expr.remove_falsy())
            }

            Expr::Object(ObjectLit { span, ref props }) => {
                let mut members = Vec::with_capacity(props.len());
                let mut special_type = None;

                for prop in props.iter() {
                    match *prop {
                        PropOrSpread::Prop(ref prop) => {
                            members.push(self.type_of_prop(&prop)?);
                        }
                        PropOrSpread::Spread(SpreadElement { ref expr, .. }) => {
                            match self.validate_expr(&expr)?.into_owned() {
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

                return Ok(Type::TypeLit(TypeLit { span, members }));
            }

            // https://github.com/Microsoft/TypeScript/issues/26959
            Expr::Yield(..) => return Ok(Type::any(span)),

            Expr::Update(..) => {
                return Ok(Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    span,
                }));
            }

            Expr::Cond(e) => {
                self.validate_cond_expr(&e)?;
            }

            Expr::Seq(e) => self.validate_seq_expr(e),

            Expr::Await(AwaitExpr { .. }) => unimplemented!("typeof(AwaitExpr)"),

            Expr::Class(ClassExpr { ref class, .. }) => {
                return Ok(self.type_of_class(None, class)?.owned());
            }

            Expr::Arrow(ref e) => return Ok(self.type_of_arrow_fn(e)?.owned()),

            Expr::Fn(FnExpr { ref function, .. }) => {
                return Ok(self.type_of_fn(&function)?.owned());
            }

            Expr::Member(ref expr) => {
                return self.type_of_member_expr(expr, mode);
            }

            Expr::MetaProp(..) => unimplemented!("typeof(MetaProp)"),

            Expr::Assign(e) => self.validate_assign_expr(e),

            Expr::TsTypeAssertion(TsTypeAssertion { ref type_ann, .. }) => {
                return Ok(Type::from(type_ann.clone()).owned());
            }

            Expr::Invalid(ref i) => return Ok(Type::any(i.span()).owned()),

            _ => unimplemented!("typeof ({:#?})", e),
        }
    }

    fn validate_assign_expr(&mut self, e: &AssignExpr) -> ValidationResult {
        match e.left {
            PatOrExpr::Pat(box Pat::Ident(ref i)) | PatOrExpr::Expr(box Expr::Ident(ref i)) => {
                // Type is any if self.declaring contains ident
            }
        }

        let mut errors = vec![];
        let span = e.span();
        e.visit_children(self);

        let rhs_ty = match self
            .validate_expr(&e.right)
            .and_then(|ty| self.expand_type(span, ty))
        {
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

    fn validate_update_expr(&mut self, e: &UpdateExpr) -> ValidationResult {
        let span = e.span;

        let res = self
            .validate_expr_with_extra(&e.arg, TypeOfMode::LValue, None)
            .and_then(|ty| self.expand_type(span, ty))
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

    fn validate_seq_expr(&mut self, e: &SeqExpr) -> ValidationResult {
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
            match self.validate_expr(e) {
                Ok(..) => {}
                Err(Error::ReferencedInInit { .. }) => {
                    is_any = true;
                }
                Err(..) => {}
            }
        }
        if is_any {
            return Ok(Type::any(span).owned());
        }

        return self.validate_expr(&exprs.last().unwrap());
    }

    pub(super) fn type_of_ident(&mut self, i: &Ident, type_mode: TypeOfMode) -> ValidationResult {
        let span = i.span();

        match i.sym {
            js_word!("arguments") => return Ok(Type::any(span).owned()),
            js_word!("Symbol") => {
                return Ok(builtin_types::get_var(self.libs, i.span, &js_word!("Symbol"))?.owned());
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
                    })
                    .owned());
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
            return Ok(ty.static_cast());
        }

        if let Some(ty) = self.find_type(&i.sym) {
            println!("({}) type_of({}): find_type", self.scope.depth(), i.sym);
            return Ok(ty.clone().respan(span).owned());
        }

        // Check `declaring` before checking variables.
        if self.scope.declaring.contains(&i.sym) {
            println!(
                "({}) reference in initialization: {}",
                self.scope.depth(),
                i.sym
            );

            if self.allow_ref_declaring {
                return Ok(Type::any(span).owned());
            } else {
                return Err(Error::ReferencedInInit { span });
            }
        }

        if let Some(ty) = self.find_var_type(&i.sym) {
            println!("({}) type_of({}): find_var_type", self.scope.depth(), i.sym);
            return Ok(ty.clone().respan(span).owned());
        }

        if let Some(_var) = self.find_var(&i.sym) {
            // TODO: Infer type or use type hint to handle
            //
            // let id: (x: Foo) => Foo = x => x;
            //
            return Ok(Type::any(span));
        }

        if let Ok(ty) = builtin_types::get_var(self.libs, span, &i.sym) {
            return Ok(ty.owned());
        }

        println!(
            "({}) type_of(): undefined symbol: {}",
            self.scope.depth(),
            i.sym,
        );

        return Err(Error::UndefinedSymbol { span: i.span });
    }

    pub(super) fn type_of_ts_entity_name(
        &self,
        span: Span,
        n: &TsEntityName,
        type_args: Option<&TsTypeParamInstantiation>,
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

fn prop_name_to_expr(key: &PropName) -> Box<Expr> {
    match *key {
        PropName::Computed(ref p) => p.expr.clone(),
        PropName::Ident(ref ident) => box Expr::Ident(ident.clone()),
        PropName::Str(ref s) => box Expr::Lit(Lit::Str(Str { ..s.clone() })),
        PropName::Num(ref s) => box Expr::Lit(Lit::Num(Number { ..s.clone() })),
    }
}
