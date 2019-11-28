use crate::{
    analyzer::{instantiate_class, Analyzer},
    errors::Error,
    ty::Type,
    util::EqIgnoreNameAndSpan,
};
use std::borrow::Cow;
use swc_common::{Fold, FoldWith, Span, Spanned};
use swc_ecma_ast::*;
use swc_ts_checker_macros::validator;

impl Fold<TsTypeAssertion> for Analyzer<'_, '_> {
    fn fold(&mut self, expr: TsTypeAssertion) -> TsTypeAssertion {
        log_fold!(expr);

        let expr = expr.fold_children(self);

        self.validate_type_cast(expr.span, &expr.expr, &expr.type_ann);

        expr
    }
}

impl Fold<TsAsExpr> for Analyzer<'_, '_> {
    fn fold(&mut self, expr: TsAsExpr) -> TsAsExpr {
        log_fold!(expr);

        let expr = expr.fold_children(self);

        self.validate_type_cast(expr.span, &expr.expr, &expr.type_ann);

        expr
    }
}

impl Analyzer<'_, '_> {
    /// ```ts
    /// var unionTuple3: [number, string | number] = [10, "foo"];
    /// var unionTuple4 = <[number, number]>unionTuple3;
    /// ```
    ///
    /// is valid, while
    ///
    /// ```ts
    /// var unionTuple3: [number, string | number] = [10, "foo"];
    /// var unionTuple4: [number, number] = unionTuple3;
    /// ```
    ///
    /// results in error.
    #[validator]
    fn validate_type_cast(&mut self, span: Span, orig: &Expr, to: &TsType) {
        let orig_ty = self.type_of(orig)?;
        let orig_ty = self.expand_type(span, orig_ty)?;

        let casted_ty = Type::from(to.clone());
        let casted_ty = self.expand_type(span, Cow::Owned(casted_ty))?;
        let casted_ty = instantiate_class(casted_ty);

        self.validate_type_cast_inner(span, &orig_ty, &casted_ty)?;
    }

    fn validate_type_cast_inner(
        &self,
        span: Span,
        orig_ty: &Type,
        casted_ty: &Type,
    ) -> Result<(), Error> {
        match *orig_ty.normalize() {
            Type::Union(ref rt) => {
                let castable = rt
                    .types
                    .iter()
                    .any(|v| casted_ty.eq_ignore_name_and_span(v));

                if castable {
                    return Ok(());
                }
            }

            _ => {}
        }

        match *casted_ty.normalize() {
            Type::Tuple(ref lt) => {
                //
                match *orig_ty.normalize() {
                    Type::Tuple(ref rt) => {
                        //
                        if lt.types.len() != rt.types.len() {
                            Err(Error::InvalidTupleCast {
                                span,
                                left: lt.span(),
                                right: rt.span(),
                            })?;
                        }

                        let mut all_castable = true;
                        //
                        for (i, lty) in lt.types.iter().enumerate() {
                            // if rt.types.len() >= i {
                            //     all_castable = false;
                            //     break;
                            // }
                            let rty = &rt.types[i];

                            let res = self.validate_type_cast_inner(span, &rty, &lty);

                            if res.is_err() {
                                all_castable = false;
                                break;
                            }
                        }

                        if all_castable {
                            return Ok(());
                        }
                    }

                    _ => {}
                }
            }

            Type::Array(ref lt) => {
                //
                match *orig_ty {
                    Type::Tuple(ref rt) => {
                        if rt.types[0].eq_ignore_name_and_span(&lt.elem_type) {
                            return Ok(());
                        }
                    }

                    // fallback to .assign
                    _ => {}
                }
            }

            // fallback to .assign
            _ => {}
        }

        self.assign(&casted_ty, &orig_ty, span)?;

        match *casted_ty {
            Type::Tuple(ref rt) => {
                //
                match *orig_ty {
                    Type::Tuple(ref lt) => {}
                    _ => {}
                }
            }
            _ => {}
        }

        Ok(())
    }
}
