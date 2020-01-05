use super::{super::Analyzer, instantiate_class};
use crate::{
    errors::Error,
    ty::Type,
    util::EqIgnoreNameAndSpan,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;

prevent!(TsTypeAssertion);
prevent!(TsAsExpr);

impl Validate<TsTypeAssertion> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &TsTypeAssertion) -> ValidationResult {
        let orig_ty = self.validate(&e.expr)?;

        self.visit_type_cast(e.span, &orig_ty, &e.type_ann)
    }
}

impl Validate<TsAsExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &TsAsExpr) -> ValidationResult {
        let orig_ty = self.validate(&e.expr)?;

        self.visit_type_cast(e.span, &orig_ty, &e.type_ann)
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
    fn validate_type_cast(&mut self, span: Span, orig_ty: &Type, to: &TsType) -> ValidationResult {
        let casted_ty = to.validate_with(self)?;
        let casted_ty = instantiate_class(casted_ty);

        self.validate_type_cast_inner(span, orig_ty, casted_ty);

        Ok(casted_ty)
    }

    fn validate_type_cast_inner(&self, span: Span, orig_ty: &Type, casted_ty: &Type) {
        match *orig_ty.normalize() {
            Type::Union(ref rt) => {
                let castable = rt
                    .types
                    .iter()
                    .any(|v| casted_ty.eq_ignore_name_and_span(v));

                if castable {
                    return;
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
                            return;
                        }
                    }

                    _ => {}
                }
            }

            Type::Array(ref lt) => {
                //
                match orig_ty {
                    Type::Tuple(ref rt) => {
                        if rt.types[0].eq_ignore_name_and_span(&lt.elem_type) {
                            return;
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

        match casted_ty {
            Type::Tuple(ref rt) => {
                //
                match orig_ty {
                    Type::Tuple(ref lt) => {}
                    _ => {}
                }
            }
            _ => {}
        }
    }
}
