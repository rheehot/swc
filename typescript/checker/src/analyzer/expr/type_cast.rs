use super::{super::Analyzer, instantiate_class};
use crate::{errors::Error, ty::Type, util::EqIgnoreNameAndSpan, ValidationResult};
use swc_common::Span;
use swc_ecma_ast::*;
use swc_ts_checker_macros::validator;

impl Analyzer<'_> {
    pub(super) fn validate_ts_type_assertion(&mut self, e: &TsAsExpr) -> ValidationResult {
        let orig_ty = self.validate_expr(&e.expr)?;

        self.validate_type_cast(e.span, &orig_ty, &e.type_ann)
    }

    pub(super) fn validate_ts_as_expr(&mut self, e: &TsAsExpr) -> ValidationResult {
        let orig_ty = self.validate_expr(&e.expr)?;

        self.validate_type_cast(e.span, &orig_ty, &e.type_ann)
    }

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
    fn validate_type_cast(&mut self, span: Span, orig_ty: &Type, to: &TsType) -> ValidationResult {
        let orig_ty = self.expand_type(span, orig_ty)?;

        let casted_ty = Type::from(to.clone());
        let casted_ty = self.expand_type(span, casted_ty.owned())?;
        let casted_ty = instantiate_class(casted_ty);

        self.validate_type_cast_inner(span, &orig_ty, &casted_ty)
    }

    fn validate_type_cast_inner(
        &self,
        span: Span,
        orig_ty: &Type,
        casted_ty: &Type,
    ) -> ValidationResult {
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
