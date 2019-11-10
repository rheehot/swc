use super::Analyzer;
use crate::{analyzer::control_flow::Comparator, errors::Error, ty::Type, util::EqIgnoreSpan};
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<BinExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, expr: &BinExpr) {
        expr.visit_children(self);

        let mut errors = vec![];
        let errs = if expr.op == op!("===") || expr.op == op!("!==") {
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
        } else if expr.op == op!(bin, "+") {
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

            if lt.is_some() && rt.is_some() {
                let c = Comparator {
                    left: &*lt.unwrap(),
                    right: &*rt.unwrap(),
                };

                if let Some(()) = c.take(|l_ty, r_ty| match l_ty.normalize() {
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
                    errors.push(Error::TS2365 { span: expr.span() })
                }
            }
        };

        self.info.errors.extend(errors);
    }
}
