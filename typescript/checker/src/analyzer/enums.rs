use super::Analyzer;
use crate::{
    analyzer::util::ResultExt,
    errors::Error,
    ty::{Enum, EnumMember, Type},
    validator::Validate,
    ValidationResult,
};
use macros::validator;
use swc_common::{Span, Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

/// We don't visit enum variants to allow
///
///        const enum E {
///            a = 10,
///            b = a,
///            c = (a+1),
///            e,
///            d = ~e,
///            f = a << 2 >> 1,
///            g = a << 2 >>> 1,
///            h = a | b
///        }
#[validator]
impl Validate<TsEnumDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<Enum>;
    #[inline(never)]
    fn validate(&mut self, e: &TsEnumDecl) -> Self::Output {
        let ty: Result<_, _> = try {
            let members = e
                .members
                .iter()
                .enumerate()
                .map(|(i, m)| -> Result<_, Error> {
                    Ok(EnumMember {
                        id: m.id.clone(),
                        val: compute(&e, i, m.init.as_ref().map(|v| &**v))?,
                        span: m.span,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            Enum {
                span: e.span,
                has_num: members.iter().any(|m| match m.val {
                    TsLit::Number(..) => true,
                    _ => false,
                }),
                has_str: members.iter().any(|m| match m.val {
                    TsLit::Str(..) => true,
                    _ => false,
                }),
                declare: e.declare,
                is_const: e.is_const,
                id: e.id.clone(),
                members,
            }
        };

        let span = e.span;

        self.register_type(
            e.id.sym.clone(),
            match ty {
                Ok(ref ty) => ty.clone().into(),
                Err(..) => Type::any(span),
            },
        )
        .store(&mut self.info.errors);

        // Validate const enums
        if e.is_const {
            for m in &e.members {
                if let Some(ref init) = m.init {
                    let mut v = LitValidator {
                        error: false,
                        decl: &e,
                    };
                    init.visit_with(&mut v);
                    if v.error {
                        self.info
                            .errors
                            .push(Error::InvalidInitInConstEnum { span: init.span() })
                    }
                }
            }
        }

        ty
    }
}

/// Called only for enums.
fn compute(e: &TsEnumDecl, i: usize, expr: Option<&Expr>) -> Result<TsLit, Error> {
    fn arithmetic_opt(
        e: &TsEnumDecl,
        span: Span,
        i: usize,
        expr: Option<&Expr>,
    ) -> Result<f64, Error> {
        if let Some(ref expr) = expr {
            return arithmetic(e, span, expr);
        }

        return Ok(i as f64);
    }

    fn arithmetic(e: &TsEnumDecl, span: Span, expr: &Expr) -> Result<f64, Error> {
        Ok(match *expr {
            Expr::Lit(ref lit) => match *lit {
                Lit::Num(ref v) => v.value,
                _ => unreachable!("arithmetic({:?})", lit),
            },
            Expr::Bin(ref bin) => arithmetic_bin(e, span, &bin)?,
            Expr::Paren(ref paren) => return arithmetic(e, span, &paren.expr),

            Expr::Ident(ref id) => {
                //
                for (i, m) in e.members.iter().enumerate() {
                    match m.id {
                        TsEnumMemberId::Str(Str { value: ref sym, .. })
                        | TsEnumMemberId::Ident(Ident { ref sym, .. }) => {
                            if *sym == id.sym {
                                return arithmetic_opt(e, span, i, m.init.as_ref().map(|v| &**v));
                            }
                        }
                    }
                }
                return Err(Error::InvalidEnumInit { span });
            }
            Expr::Unary(ref expr) => {
                let v = arithmetic(e, span, &expr.arg)?;

                match expr.op {
                    op!(unary, "+") => return Ok(v),
                    op!(unary, "-") => return Ok(-v),
                    op!("!") => return Ok(if v == 0.0f64 { 0.0 } else { 1.0 }),
                    op!("~") => return Ok((!(v as u32)) as f64),
                    _ => return Err(Error::InvalidEnumInit { span }),
                };
            }
            _ => Err(Error::InvalidEnumInit { span })?,
        })
    }

    fn arithmetic_bin(e: &TsEnumDecl, span: Span, expr: &BinExpr) -> Result<f64, Error> {
        let l = arithmetic(e, span, &expr.left)?;
        let r = arithmetic(e, span, &expr.right)?;

        Ok(match expr.op {
            op!(bin, "+") => l + r,
            op!(bin, "-") => l - r,
            op!("*") => l * r,
            op!("/") => l / r,

            // TODO
            op!("&") => ((l.round() as i64) & (r.round() as i64)) as _,
            op!("|") => ((l.round() as i64) | (r.round() as i64)) as _,
            op!("^") => ((l.round() as i64) ^ (r.round() as i64)) as _,

            op!("<<") => ((l.round() as i64) << (r.round() as i64)) as _,
            op!(">>") => ((l.round() as i64) >> (r.round() as i64)) as _,
            // TODO: Verify this
            op!(">>>") => ((l.round() as u64) >> (r.round() as u64)) as _,
            _ => unimplemented!("arithmetic_bin({:?})", expr.op),
        })
    }

    fn try_str(e: &Expr) -> Result<Str, ()> {
        match *e {
            Expr::Lit(Lit::Str(ref s)) => return Ok(s.clone()),
            _ => Err(()),
        }
    }

    if let Some(ref expr) = expr {
        if let Ok(s) = try_str(&expr) {
            return Ok(s.into());
        }
    }

    Ok(Number {
        span: expr.span(),
        value: arithmetic_opt(e, e.span, i, expr)?,
    }
    .into())
}

impl Analyzer<'_, '_> {
    // Check for constant enum in rvalue.
    pub(super) fn check_rvalue(&mut self, rhs_ty: &Type) {
        match *rhs_ty.normalize() {
            Type::Enum(ref e) if e.is_const => {
                self.info
                    .errors
                    .push(Error::ConstEnumUsedAsVar { span: e.span() });
            }
            _ => {}
        }
    }

    pub(super) fn expand_enum_variant(&self, ty: Type) -> Result<Type, Error> {
        match ty.normalize() {
            Type::EnumVariant(ref v) => {
                if let Some(types) = self.find_type(&v.enum_name) {
                    for ty in types {
                        if let Type::Enum(Enum { members, .. }) = ty {
                            if let Some(v) = members.iter().find(|m| match m.id {
                                TsEnumMemberId::Ident(Ident { ref sym, .. })
                                | TsEnumMemberId::Str(Str { value: ref sym, .. }) => *sym == v.name,
                            }) {
                                return Ok(Type::Lit(TsLitType {
                                    span: v.span,
                                    lit: v.val.clone(),
                                }));
                            }
                        }
                    }
                }
            }
            _ => {}
        }

        return Ok(ty);
    }
}

struct LitValidator<'a> {
    decl: &'a TsEnumDecl,
    error: bool,
}

impl Visit<Expr> for LitValidator<'_> {
    fn visit(&mut self, e: &Expr) {
        e.visit_children(self);

        match e {
            Expr::Lit(..) => {}
            Expr::Ident(ref i) => {
                let is_ref = self.decl.members.iter().any(|m| match m.id {
                    TsEnumMemberId::Ident(Ident { ref sym, .. })
                    | TsEnumMemberId::Str(Str { value: ref sym, .. }) => *sym == i.sym,
                });
                if !is_ref {
                    self.error = true;
                    return;
                }
            }
            Expr::Unary(..) | Expr::Bin(..) | Expr::Paren(..) => {}

            _ => {
                self.error = true;
                return;
            }
        }
    }
}
