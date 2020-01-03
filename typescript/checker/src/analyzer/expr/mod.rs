use super::Analyzer;
use crate::{
    analyzer::util::{Comparator, ResultExt},
    errors::Error,
    ty::{ClassInstance, Tuple, Type, TypeLit, TypeParamInstantiation, TypeRef},
    util::{EqIgnoreSpan, IntoCow, RemoveTypes},
    ValidationResult,
};
use swc_atoms::js_word;
use swc_common::Spanned;
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

impl Analyzer<'_> {
    pub(super) fn validate_expr(&mut self, e: &Expr) -> ValidationResult {
        self.validate_expr_with_extra(e, TypeOfMode::RValue, None)
    }

    pub(super) fn validate_expr_with_extra(
        &mut self,
        e: &Expr,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
    ) -> Result<TypeRef, Error> {
        let span = e.span();

        match e {
            // super() returns any
            Expr::Call(CallExpr {
                callee: ExprOrSuper::Super(..),
                ..
            }) => Ok(Type::any(span).into_cow()),

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

            Expr::Ident(ref i) => self.type_of_ident(i, type_mode),

            Expr::Array(ArrayLit { ref elems, .. }) => {
                let mut types: Vec<TypeRef> = Vec::with_capacity(elems.len());

                for elem in elems {
                    let span = elem.span();
                    match elem {
                        Some(ExprOrSpread {
                            spread: None,
                            ref expr,
                        }) => {
                            let ty = self.type_of(expr)?;
                            types.push(ty)
                        }
                        Some(ExprOrSpread {
                            spread: Some(..), ..
                        }) => unimplemented!("type of array spread"),
                        None => {
                            let ty = Type::undefined(span);
                            types.push(ty.into_cow())
                        }
                    }
                }

                return Ok(Type::Tuple(Tuple { span, types }).into_cow());
            }

            Expr::Lit(Lit::Bool(v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Bool(v),
                })
                .into_cow());
            }
            Expr::Lit(Lit::Str(ref v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Str(v.clone()),
                })
                .into_cow());
            }
            Expr::Lit(Lit::Num(v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Number(v),
                })
                .into_cow());
            }
            Expr::Lit(Lit::Null(Null { span })) => {
                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNullKeyword,
                })
                .into_cow());
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
                })
                .into_cow());
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
                    })
                    .into_cow());
                }

                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                })
                .into_cow());
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
                            match self.type_of(&expr)?.into_owned() {
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
                    return Ok(ty.into_cow());
                }

                return Ok(Type::TypeLit(TypeLit { span, members }).into_cow());
            }

            // https://github.com/Microsoft/TypeScript/issues/26959
            Expr::Yield(..) => return Ok(Type::any(span).into_cow()),

            Expr::Update(..) => {
                return Ok(Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    span,
                })
                .into_cow());
            }

            Expr::Cond(CondExpr {
                ref test,
                ref cons,
                ref alt,
                ..
            }) => {
                match **test {
                    Expr::Ident(ref i) => {
                        // Check `declaring` before checking variables.
                        if self.declaring.contains(&i.sym) {
                            if self.allow_ref_declaring {
                                return Ok(Type::any(span).owned());
                            } else {
                                return Err(Error::ReferencedInInit { span });
                            }
                        }
                    }
                    _ => {}
                }

                let cons_ty = self.type_of(cons)?.into_owned();
                let alt_ty = self.type_of(alt)?.into_owned();

                return Ok(Type::union(once(cons_ty).chain(once(alt_ty))).into_cow());
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
                return self.type_of_member_expr(expr, type_mode);
            }

            Expr::MetaProp(..) => unimplemented!("typeof(MetaProp)"),

            Expr::Assign(AssignExpr {
                left: PatOrExpr::Pat(box Pat::Ident(ref i)),
                ref right,
                ..
            }) => {
                if self.declaring.contains(&i.sym) {
                    return Ok(Type::any(span).owned());
                }

                return self.type_of(right);
            }

            Expr::Assign(AssignExpr {
                left: PatOrExpr::Expr(ref left),
                ref right,
                ..
            }) => {
                match **left {
                    Expr::Ident(ref i) => {
                        if self.declaring.contains(&i.sym) {
                            return Ok(Type::any(span).owned());
                        }
                    }
                    _ => {}
                }

                return self.type_of(right);
            }

            Expr::Assign(AssignExpr { ref right, .. }) => return self.type_of(right),

            Expr::TsTypeAssertion(TsTypeAssertion { ref type_ann, .. }) => {
                return Ok(Type::from(type_ann.clone()).owned());
            }

            Expr::Invalid(ref i) => return Ok(Type::any(i.span()).owned()),

            _ => unimplemented!("typeof ({:#?})", e),
        }
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
        })
        .into_cow())
    }

    fn validate_seq_expr(&mut self, e: &SeqExpr) -> ValidationResult {
        let SeqExpr { span, ref exprs } = *e;

        assert!(exprs.len() >= 1);

        let mut is_any = false;
        for e in exprs.iter() {
            match **e {
                Expr::Ident(ref i) => {
                    if self.declaring.contains(&i.sym) {
                        is_any = true;
                    }
                }
                _ => {}
            }
            match self.type_of(e) {
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

        return self.type_of(&exprs.last().unwrap());
    }
}

fn instantiate_class(ty: TypeRef) -> TypeRef {
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
        })
        .owned(),

        Type::Class(ref cls) => Type::ClassInstance(ClassInstance {
            // TODO
            span,

            // TODO; Remove clone
            cls: cls.clone(),

            // TODO
            type_args: None,
        })
        .owned(),
        _ => ty,
    }
}
