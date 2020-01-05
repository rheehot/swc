use super::super::Analyzer;
use crate::{errors::Error, ty::Type, validator::Validate, ValidationResult};
use swc_atoms::js_word;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;

prevent!(UnaryExpr);

impl Validate<UnaryExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &UnaryExpr) -> Self::Output {
        let UnaryExpr { span, op, ref arg } = *e;

        let mut errors = vec![];

        let arg = self.validate(&arg).store(&mut errors);

        self.info.errors.extend(errors.drain(..));

        if let Some(arg) = arg {
            self.validate_unary_expr_inner(span, op, &arg);
        }

        match op {
            op!("typeof") => {
                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                }));
            }

            // `delete foo` returns bool
            op!("delete") => {
                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsBooleanKeyword,
                }));
            }

            op!("void") => return Ok(Type::undefined(span)),

            _ => {}
        }

        let arg_ty = self.validate(arg)?;
        match *arg_ty {
            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Err(Error::Unknown { span: arg.span() }),
            _ => {}
        }

        match op {
            op!("!") => return Ok(negate(self.validate(arg)?.into_owned())),

            op!(unary, "-") | op!(unary, "+") => {
                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                }));
            }

            op!("~") => {
                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                }));
            }

            op!("typeof") | op!("delete") | op!("void") => unreachable!(),
        }
    }
}

impl Analyzer<'_, '_> {
    fn validate_unary_expr_inner(&mut self, span: Span, op: UnaryOp, arg: &Type) {
        let mut errors = vec![];

        match op {
            op!("typeof") | op!("delete") | op!("void") => match arg.normalize() {
                Type::EnumVariant(..) if op == op!("delete") => {
                    errors.push(Error::TS2704 { span: arg.span() })
                }

                _ => {}
            },

            op!("~") | op!(unary, "-") | op!(unary, "+") => match arg.normalize() {
                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                }) => {}

                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsNullKeyword,
                    ..
                }) => errors.push(Error::TS2531 { span: arg.span() }),

                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsUndefinedKeyword,
                    ..
                }) => errors.push(Error::TS2532 { span: arg.span() }),

                _ => {
                    //
                }
            },

            _ => {}
        }

        self.info.errors.extend(errors);
    }
}

fn negate(ty: Type) -> Type {
    match ty {
        Type::Lit(TsLitType { ref lit, span }) => match *lit {
            TsLit::Bool(v) => {
                return Type::Lit(TsLitType {
                    lit: TsLit::Bool(Bool {
                        value: !v.value,
                        ..v
                    }),
                    span,
                });
            }
            TsLit::Number(v) => {
                return Type::Lit(TsLitType {
                    lit: TsLit::Bool(Bool {
                        value: v.value != 0.0,
                        span: v.span,
                    }),
                    span,
                });
            }
            TsLit::Str(ref v) => {
                return Type::Lit(TsLitType {
                    lit: TsLit::Bool(Bool {
                        value: v.value != js_word!(""),
                        span: v.span,
                    }),
                    span,
                });
            }
        },

        _ => {}
    }

    TsKeywordType {
        span: ty.span(),
        kind: TsKeywordTypeKind::TsBooleanKeyword,
    }
    .into()
}
