use super::Analyzer;
use crate::{
    analyzer::util::{Comparator, ResultExt},
    errors::Error,
    ty::{Type, TypeParamInstantiation, TypeRef},
    util::{EqIgnoreSpan, IntoCow},
};
use swc_common::Spanned;
use swc_ecma_ast::*;

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
    pub(super) fn validate_expr(&mut self, e: &Expr) -> Result<TypeRef, Error> {
        self.validate_expr_with_extra(e, TypeOfMode::RValue, None)
    }

    pub(super) fn validate_expr_with_extra(
        &mut self,
        e: &Expr,
        mode: TypeOfMode,
        type_args: Option<&TypeParamInstantiation>,
    ) -> Result<TypeRef, Error> {
        match e {}
    }

    fn validate_bin_expr(&mut self, e: &BinExpr) -> Result<TypeRef, Error> {
        let BinExpr {
            span,
            ref left,
            op,
            ref right,
        } = *e;

        let mut errors = vec![];

        let lt = self.validate_expr(&left).store(&mut errors);
        let rt = self.validate_expr(&right).store(&mut errors);

        macro_rules! no_unknown {
            () => {{
                no_unknown!(lt);
                no_unknown!(rt);
            }};
            ($ty:expr) => {{
                match *$ty {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => {
                        return Err(Error::Unknown { span });
                    }
                    _ => {}
                }
            }};
        }

        let ls = lt.span();
        let rs = rt.span();

        let ty = match e.op {
            op!("===") | op!("!==") | op!("!=") | op!("==") => {
                if lt.is_some() && rt.is_some() {
                    let lt = lt.unwrap();
                    let rt = rt.unwrap();

                    let has_overlap = lt.eq_ignore_span(&*rt) || {
                        let c = Comparator {
                            left: &*lt,
                            right: &*rt,
                        };

                        // Check if type overlaps.
                        match c.take(|l, r| {
                            // Returns Some(()) if r may be assignable to l
                            match l {
                                Type::Lit(ref l_lit) => {
                                    // "foo" === "bar" is always false.
                                    match r {
                                        Type::Lit(ref r_lit) => {
                                            if l_lit.eq_ignore_span(&*r_lit) {
                                                Some(())
                                            } else {
                                                None
                                            }
                                        }
                                        _ => Some(()),
                                    }
                                }
                                Type::Union(ref u) => {
                                    // Check if u contains r

                                    Some(())
                                }
                                _ => None,
                            }
                        }) {
                            Some(()) => true,
                            None => false,
                        }
                    };

                    if !has_overlap {
                        errors.push(Error::NoOverlap {
                            span: e.span(),
                            value: expr.op != op!("==="),
                            left: ls,
                            right: rs,
                        })
                    }
                }

                Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                })
                .owned()
            }
            op!(bin, "+") => {
                no_unknown!();

                let c = Comparator {
                    left: (&**left, &l_ty),
                    right: (&**right, &r_ty),
                };

                if let Some(()) = c.take(|(_, l_ty), (_, _)| match **l_ty {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => Some(()),

                    _ => None,
                }) {
                    errors.push(Error::Unknown { span });
                }

                match *l_ty {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    })
                    | Type::Lit(TsLitType {
                        lit: TsLit::Number(..),
                        ..
                    }) => match *r_ty {
                        Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                            ..
                        })
                        | Type::Lit(TsLitType {
                            lit: TsLit::Number(..),
                            ..
                        }) => {
                            return Ok(Type::Keyword(TsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsStringKeyword,
                            })
                            .owned());
                        }
                        _ => {}
                    },
                    _ => {}
                }

                if let Some(()) = c.take(|(_, l_ty), (_, _)| match **l_ty {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    })
                    | Type::Lit(TsLitType {
                        lit: TsLit::Str(..),
                        ..
                    }) => Some(()),

                    _ => None,
                }) {
                    return Ok(Type::Keyword(TsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsStringKeyword,
                    })
                    .owned());
                } else if let Some(kind) = c.take(|(_, l_ty), (_, r_ty)| {
                    // Rule:
                    //  - any + string is string
                    //  - any + other is any

                    if l_ty.is_any() {
                        if r_ty.is_str() {
                            return Some(TsKeywordTypeKind::TsStringKeyword);
                        }
                        return Some(TsKeywordTypeKind::TsAnyKeyword);
                    }

                    None
                }) {
                    Ok(Type::Keyword(TsKeywordType { span, kind }).owned());
                } else if c.any(|(_, ty)| {
                    ty.is_keyword(TsKeywordTypeKind::TsUndefinedKeyword)
                        || ty.is_keyword(TsKeywordTypeKind::TsNullKeyword)
                }) {
                    return Err(Error::TS2365 { span });
                } else if c.both(|(_, ty)| {
                    // Rule:
                    //  - null is invalid operand
                    //  - undefined is invalid operand
                    match **ty {
                        Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsUndefinedKeyword,
                            ..
                        })
                        | Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsNullKeyword,
                            ..
                        }) => true,

                        _ => false,
                    }
                }) {
                    errors.push(Error::TS2365 { span });
                } else if let Some(()) = c.take(|(_, l_ty), (_, r_ty)| match l_ty.normalize() {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsBooleanKeyword,
                        ..
                    }) => match r_ty.normalize() {
                        Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                            ..
                        }) => Some(()),
                        _ => None,
                    },
                    _ => None,
                }) {
                    Type::Keyword(TsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsBooleanKeyword,
                    })
                    .owned()
                } else {
                }
            }
            op!("||") | op!("&&") => {
                let lt = self.type_of(&expr.left).ok();

                if lt.is_some() {
                    match *lt.as_ref().unwrap().normalize() {
                        Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsVoidKeyword,
                            ..
                        }) => errors.push(Error::TS1345 { span: expr.span() }),
                        _ => {}
                    }
                }

                match lt.normalize() {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => return Ok(Type::any(span).owned()),

                    _ => {}
                }

                rt
            }

            op!("*")
            | op!("/")
            | op!("%")
            | op!(bin, "-")
            | op!("<<")
            | op!(">>")
            | op!(">>>")
            | op!("&")
            | op!("^")
            | op!("|") => {
                if lt.is_some() && rt.is_some() {
                    let lt = lt.unwrap();
                    let rt = rt.unwrap();

                    let mut check = |ty: &Type, is_left| match ty {
                        Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        })
                        | Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                            ..
                        })
                        | Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsBigIntKeyword,
                            ..
                        })
                        | Type::Lit(TsLitType {
                            lit: TsLit::Number(..),
                            ..
                        })
                        | Type::Enum(..)
                        | Type::EnumVariant(..) => {}

                        _ => errors.push(if is_left {
                            Error::TS2362 { span: ty.span() }
                        } else {
                            Error::TS2363 { span: ty.span() }
                        }),
                    };

                    if (expr.op == op!("&") || expr.op == op!("^") || expr.op == op!("|"))
                        && match lt.normalize() {
                            Type::Keyword(TsKeywordType {
                                kind: TsKeywordTypeKind::TsBooleanKeyword,
                                ..
                            })
                            | Type::Lit(TsLitType {
                                lit: TsLit::Bool(..),
                                ..
                            }) => true,
                            _ => false,
                        }
                        && match rt.normalize() {
                            Type::Keyword(TsKeywordType {
                                kind: TsKeywordTypeKind::TsBooleanKeyword,
                                ..
                            })
                            | Type::Lit(TsLitType {
                                lit: TsLit::Bool(..),
                                ..
                            }) => true,
                            _ => false,
                        }
                    {
                        errors.push(Error::TS2447 { span: expr.span() });
                    } else {
                        check(&lt, true);
                        check(&rt, false);
                    }
                }

                Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                })
                .owned()
            }

            op!("in") => {
                let lt = self.type_of(&expr.left).ok();
                let rt = self.type_of(&expr.right).ok();

                if lt.is_some() {
                    match lt.unwrap().normalize() {
                        Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsAnyKeyword,
                            ..
                        })
                        | Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsStringKeyword,
                            ..
                        })
                        | Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                            ..
                        })
                        | Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsBigIntKeyword,
                            ..
                        })
                        | Type::Lit(TsLitType {
                            lit: TsLit::Number(..),
                            ..
                        })
                        | Type::Lit(TsLitType {
                            lit: TsLit::Str(..),
                            ..
                        })
                        | Type::Enum(..)
                        | Type::EnumVariant(..) => {}

                        _ => errors.push(Error::TS2360 {
                            span: expr.left.span(),
                        }),
                    }
                }

                if rt.is_some() {
                    fn is_ok(ty: &Type) -> bool {
                        if ty.is_any() {
                            true;
                        }

                        match ty.normalize() {
                            Type::TypeLit(..)
                            | Type::Param(..)
                            | Type::Array(..)
                            | Type::Tuple(..)
                            | Type::Interface(..)
                            | Type::Keyword(TsKeywordType {
                                kind: TsKeywordTypeKind::TsObjectKeyword,
                                ..
                            }) => true,
                            Type::Union(ref u) => u.types.iter().all(|ty| is_ok(&ty)),
                            _ => false,
                        }
                    }

                    if !is_ok(&rt.unwrap()) {
                        errors.push(Error::TS2361 {
                            span: expr.right.span(),
                        })
                    }
                }

                Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                })
                .owned()
            }

            _ => {}
        };

        self.info.errors.extend(errors);

        Ok(ty)
    }

    fn validate_update_expr(&mut self, e: &UpdateExpr) -> Result<TypeRef, Error> {
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
}
