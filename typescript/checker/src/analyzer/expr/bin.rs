use super::Analyzer;
use crate::{
    analyzer::control_flow::Comparator,
    errors::Error,
    ty::{Type, TypeRef},
    util::{EqIgnoreSpan, IntoCow},
};
use swc_common::{Fold, FoldWith, Spanned};
use swc_ecma_ast::*;

impl Fold<BinExpr> for Analyzer<'_, '_> {
    fn fold(&mut self, expr: BinExpr) -> BinExpr {
        let expr = expr.fold_children(self);

        let mut errors = vec![];

        match expr.op {
            op!("===") | op!("!==") => {
                let lt = match self.type_of(&expr.left) {
                    Ok(lt) => Some(lt),
                    Err(err) => {
                        errors.push(err);
                        None
                    }
                };

                let rt = match self.type_of(&expr.right) {
                    Ok(rt) => Some(rt),
                    Err(err) => {
                        errors.push(err);
                        None
                    }
                };

                let ls = lt.span();
                let rs = rt.span();

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
                            span: expr.span(),
                            value: expr.op != op!("==="),
                            left: ls,
                            right: rs,
                        })
                    }
                }
            }
            op!(bin, "+") => {
                // Validation is performed in type_of_bin_expr because
                // validation of types is required to compute type of the
                // expression.
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
                let lt = self.type_of(&expr.left).ok();
                let rt = self.type_of(&expr.right).ok();

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
                            return true;
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
            }

            _ => {}
        }

        self.info.errors.extend(errors);

        expr
    }
}

impl Analyzer<'_, '_> {
    pub(super) fn type_of_bin_expr(&self, expr: &BinExpr) -> Result<TypeRef, Error> {
        let BinExpr {
            span,
            op,
            ref left,
            ref right,
        } = *expr;

        let l_ty = self
            .type_of(&left)
            .and_then(|ty| self.expand_enum_variant(ty));
        let r_ty = self
            .type_of(&right)
            .and_then(|ty| self.expand_enum_variant(ty));

        let (l_ty, r_ty) = match (l_ty, r_ty) {
            (Ok(l), Ok(r)) => (l, r),
            (Err(e), Ok(_)) | (Ok(_), Err(e)) => return Err(e),
            (Err(l), Err(r)) => {
                return Err(Error::Errors {
                    span,
                    errors: vec![l, r],
                })
            }
        };

        macro_rules! no_unknown {
            () => {{
                no_unknown!(l_ty);
                no_unknown!(r_ty);
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

        match op {
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
                    return Err(Error::Unknown { span });
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
                }

                // Rule:
                //  - any + string is string
                //  - any + other is any
                if let Some(kind) = c.take(|(_, l_ty), (_, r_ty)| {
                    if l_ty.is_any() {
                        if r_ty.is_str() {
                            return Some(TsKeywordTypeKind::TsStringKeyword);
                        }
                        return Some(TsKeywordTypeKind::TsAnyKeyword);
                    }

                    None
                }) {
                    return Ok(Type::Keyword(TsKeywordType { span, kind }).owned());
                }

                if c.any(|(_, ty)| {
                    ty.is_keyword(TsKeywordTypeKind::TsUndefinedKeyword)
                        || ty.is_keyword(TsKeywordTypeKind::TsNullKeyword)
                }) {
                    return Err(Error::TS2365 { span });
                }

                // Rule:
                //  - null is invalid operand
                //  - undefined is invalid operand
                if c.both(|(_, ty)| match **ty {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        ..
                    })
                    | Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        ..
                    }) => true,

                    _ => false,
                }) {
                    return Err(Error::TS2365 { span });
                }

                if let Some(()) = c.take(|(_, l_ty), (_, r_ty)| match l_ty.normalize() {
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
                    return Ok(Type::Keyword(TsKeywordType {
                        span,
                        kind: TsKeywordTypeKind::TsBooleanKeyword,
                    })
                    .owned());
                }

                unimplemented!("type_of_bin(+)\nLeft: {:#?}\nRight: {:#?}", l_ty, r_ty)
            }
            op!("*") | op!("/") => {
                no_unknown!();

                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                })
                .owned());
            }

            op!(bin, "-")
            | op!("<<")
            | op!(">>")
            | op!(">>>")
            | op!("%")
            | op!("|")
            | op!("&")
            | op!("^")
            | op!("**") => {
                no_unknown!();

                return Ok(Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    span,
                })
                .into_cow());
            }

            op!("===") | op!("!==") | op!("!=") | op!("==") => {
                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                })
                .owned());
            }

            op!("<=") | op!("<") | op!(">=") | op!(">") | op!("in") | op!("instanceof") => {
                no_unknown!();

                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                })
                .owned());
            }

            op!("||") | op!("&&") => {
                no_unknown!();

                match l_ty.normalize() {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => return Ok(Type::any(span).owned()),

                    _ => {}
                }

                return Ok(r_ty);
            }
        }
    }
}
