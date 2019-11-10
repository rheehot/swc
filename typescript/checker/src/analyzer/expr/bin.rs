use super::Analyzer;
use crate::{
    analyzer::control_flow::Comparator,
    errors::Error,
    ty::{Type, TypeRef},
    util::EqIgnoreSpan,
};
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<BinExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, expr: &BinExpr) {
        expr.visit_children(self);

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
                let lt = self.type_of(&expr.left).ok();
                let rt = self.type_of(&expr.right).ok();

                if lt.is_some() && rt.is_some() {
                    let c = Comparator {
                        left: &*lt.unwrap(),
                        right: &*rt.unwrap(),
                    };

                    if let Some(()) = c.take(|l_ty, r_ty| match l_ty.normalize() {
                        Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsVoidKeyword,
                            ..
                        }) => Some(()),
                        _ => None,
                    }) {
                        errors.push(Error::TS1345 { span: expr.span() })
                    }
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
    }
}
