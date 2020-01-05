use super::Analyzer;
use crate::{
    errors::Error,
    ty,
    ty::{
        Alias, Array, CallSignature, Conditional, ConstructorSignature, Enum, EnumMember,
        IndexSignature, Interface, Intersection, Mapped, MethodSignature, Operator,
        PropertySignature, TsExpr, Tuple, Type, TypeElement, TypeLit, TypeParam, TypeParamDecl,
        TypeParamInstantiation, Union,
    },
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use swc_common::Spanned;
use swc_ecma_ast::*;

impl Validate<TsTypeParamDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeParamDecl>;

    fn validate(&mut self, decl: &TsTypeParamDecl) -> Self::Output {
        Ok(TypeParamDecl {
            span: decl.span,
            params: self.validate(&decl.params)?,
        })
    }
}

impl Validate<TsTypeParam> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeParam>;

    fn validate(&mut self, p: &TsTypeParam) -> Self::Output {
        Ok(TypeParam {
            span: p.span,
            name: p.name.sym,
            constraint: try_opt!(self.validate(&p.constraint)).map(Box::new),
            default: try_opt!(self.validate(&p.default)).map(Box::new),
        })
    }
}

impl Validate<TsTypeAnn> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    #[inline]
    fn validate(&mut self, ann: &TsTypeAnn) -> Self::Output {
        self.validate(&ann.type_ann)
    }
}

impl Validate<TsTypeAliasDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<Alias>;

    fn validate(&mut self, d: &TsTypeAliasDecl) -> Self::Output {
        Ok(Alias {
            span: d.span,
            ty: box self.validate(&d.type_ann)?,
            type_params: try_opt!(self.validate(&d.type_params)),
        })
    }
}

impl Validate<TsInterfaceDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<Interface>;

    fn validate(&mut self, d: &TsInterfaceDecl) -> Self::Output {
        Ok(Interface {
            span: d.span,
            name: d.id.sym,
            type_params: try_opt!(self.validate(&d.type_params)),
            extends: self.validate(&d.extends)?,
            body: self.validate(&d.body.body)?,
        })
    }
}

impl Validate<TsTypeLit> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeLit>;

    fn validate(&mut self, lit: &TsTypeLit) -> Self::Output {
        Ok(TypeLit {
            span: lit.span,
            members: self.validate(&lit.members)?,
        })
    }
}

impl Validate<TsTypeElement> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeElement>;

    fn validate(&mut self, e: &TsTypeElement) -> Self::Output {
        Ok(match e {
            TsTypeElement::TsCallSignatureDecl(d) => TypeElement::Call(self.validate(d)?),
            TsTypeElement::TsConstructSignatureDecl(d) => {
                TypeElement::Constructor(self.validate(d)?)
            }
            TsTypeElement::TsIndexSignature(d) => TypeElement::Index(self.validate(d)?),
            TsTypeElement::TsMethodSignature(d) => TypeElement::Method(self.validate(d)?),
            TsTypeElement::TsPropertySignature(d) => TypeElement::Property(self.validate(d)?),
        })
    }
}

impl Validate<TsConstructSignatureDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<ConstructorSignature>;

    fn validate(&mut self, d: &TsConstructSignatureDecl) -> Self::Output {
        Ok(ConstructorSignature {
            span: d.span,
            params: self.validate(&d.params)?,
            type_params: try_opt!(self.validate(&d.type_params)),
            ret_ty: try_opt!(self.validate(&d.type_ann)),
        })
    }
}

impl Validate<TsCallSignatureDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<CallSignature>;

    fn validate(&mut self, d: &TsCallSignatureDecl) -> Self::Output {
        Ok(CallSignature {
            span: d.span,
            params: self.validate(&d.params)?,
            type_params: try_opt!(self.validate(&d.type_params)),
            ret_ty: try_opt!(self.validate(&d.type_ann)),
        })
    }
}

impl Validate<TsMethodSignature> for Analyzer<'_, '_> {
    type Output = ValidationResult<MethodSignature>;

    fn validate(&mut self, d: &TsMethodSignature) -> Self::Output {
        Ok(MethodSignature {
            span: d.span,
            readonly: d.readonly,
            key: d.key,
            computed: d.computed,
            optional: d.optional,
            params: self.validate(&d.params)?,
            ret_ty: try_opt!(self.validate(&d.type_ann)),
            type_params: try_opt!(self.validate(&d.type_params)),
        })
    }
}

impl Validate<TsIndexSignature> for Analyzer<'_, '_> {
    type Output = ValidationResult<IndexSignature>;

    fn validate(&mut self, d: &TsIndexSignature) -> Self::Output {
        Ok(IndexSignature {
            span: d.span,
            params: self.validate(&d.params)?,
            readonly: d.readonly,
            type_ann: try_opt!(self.validate(&d.type_ann)),
        })
    }
}

impl Validate<TsPropertySignature> for Analyzer<'_, '_> {
    type Output = ValidationResult<PropertySignature>;

    fn validate(&mut self, d: &TsPropertySignature) -> Self::Output {
        Ok(PropertySignature {
            span: d.span,
            computed: d.computed,
            key: d.key,
            optional: d.optional,
            params: self.validate(&d.params)?,
            readonly: d.readonly,
            type_ann: try_opt!(self.validate(&d.type_ann)),
            type_params: try_opt!(self.validate(&d.type_params)),
        })
    }
}

impl Validate<TsExprWithTypeArgs> for Analyzer<'_, '_> {
    type Output = ValidationResult<TsExpr>;

    fn validate(&mut self, e: &TsExprWithTypeArgs) -> Self::Output {
        Ok(TsExpr {
            span: e.span,
            expr: e.expr,
            type_args: try_opt!(e.type_args.validate_with(self)),
        })
    }
}

impl Validate<TsTypeParamInstantiation> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeParamInstantiation>;

    fn validate(&mut self, i: &TsTypeParamInstantiation) -> Self::Output {
        Ok(TypeParamInstantiation {
            span: i.span,
            params: i.params.validate_with(self)?,
        })
    }
}

impl Validate<TsTupleType> for Analyzer<'_, '_> {
    type Output = ValidationResult<Tuple>;

    fn validate(&mut self, t: &TsTupleType) -> Self::Output {
        Ok(Tuple {
            span: t.span,
            types: t.elem_types.validate_with(self)?,
        })
    }
}

impl Validate<TsConditionalType> for Analyzer<'_, '_> {
    type Output = ValidationResult<Conditional>;

    fn validate(&mut self, t: &TsConditionalType) -> Self::Output {
        Ok(Conditional {
            span: t.span,
            check_type: box t.check_type.validate_with(self)?,
            extends_type: box t.extends_type.validate_with(self)?,
            true_type: box t.true_type.validate_with(self)?,
            false_type: box t.false_type.validate_with(self)?,
        })
    }
}

impl Validate<TsMappedType> for Analyzer<'_, '_> {
    type Output = ValidationResult<Mapped>;

    fn validate(&mut self, ty: &TsMappedType) -> Self::Output {
        Ok(Mapped {
            span: ty.span,
            readonly: ty.readonly,
            optional: ty.optional,
            type_param: ty.type_param.validate_with(self)?,
            ty: try_opt!(ty.type_ann.validate_with(self)).map(Box::new),
        })
    }
}

impl Validate<TsTypeOperator> for Analyzer<'_, '_> {
    type Output = ValidationResult<Operator>;

    fn validate(&mut self, ty: &TsTypeOperator) -> Self::Output {
        Ok(Operator {
            span: ty.span,
            op: ty.op,
            ty: box ty.type_ann.validate_with(self)?,
        })
    }
}

impl Validate<TsEnumDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<Enum>;

    fn validate(&mut self, e: &TsEnumDecl) -> Self::Output {
        let members = e
            .members
            .iter()
            .enumerate()
            .map(|(i, m)| {
                Ok(EnumMember {
                    id: m.id.clone(),
                    val: compute(&e, i, m.init.as_ref().map(|v| &**v))?,
                    span: m.span,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Enum {
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
            id: e.id,
            members,
        })
    }
}

/// Called only for enums.
fn compute(e: &TsEnumDecl, i: usize, expr: Option<&Expr>) -> Result<TsLit, Error> {
    fn arithmetic_opt(e: &TsEnumDecl, i: usize, expr: Option<&Expr>) -> Result<f64, Error> {
        if let Some(ref expr) = expr {
            return arithmetic(e, expr);
        }

        return Ok(i as f64);
    }

    fn arithmetic(e: &TsEnumDecl, expr: &Expr) -> Result<f64, Error> {
        Ok(match *expr {
            Expr::Lit(ref lit) => match *lit {
                Lit::Num(ref v) => v.value,
                _ => unreachable!("arithmetic({:?})", lit),
            },
            Expr::Bin(ref bin) => arithmetic_bin(e, &bin)?,

            Expr::Ident(ref id) => {
                //
                for (i, m) in e.members.iter().enumerate() {
                    match m.id {
                        TsEnumMemberId::Str(Str { value: ref sym, .. })
                        | TsEnumMemberId::Ident(Ident { ref sym, .. }) => {
                            if *sym == id.sym {
                                return arithmetic_opt(e, i, m.init.as_ref().map(|v| &**v));
                            }
                        }
                    }
                }

                return Err(Error::InvalidEnumInit { span: expr.span() });
            }
            Expr::Unary(..) => unimplemented!("compute(unary, {:?})", expr),
            _ => return Err(Error::InvalidEnumInit { span: expr.span() }),
        })
    }

    fn arithmetic_bin(e: &TsEnumDecl, expr: &BinExpr) -> Result<f64, Error> {
        let l = arithmetic(e, &expr.left)?;
        let r = arithmetic(e, &expr.right)?;

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
        value: arithmetic_opt(e, i, expr)?,
    }
    .into())
}

impl Validate<TsArrayType> for Analyzer<'_, '_> {
    type Output = ValidationResult<Array>;

    fn validate(&mut self, node: &TsArrayType) -> Self::Output {
        Ok(Array {
            span: node.span,
            elem_type: box node.elem_type.validate_with(self)?,
        })
    }
}

impl Validate<TsUnionType> for Analyzer<'_, '_> {
    type Output = ValidationResult<Union>;

    fn validate(&mut self, u: &TsUnionType) -> Self::Output {
        Ok(Union {
            span: u.span,
            types: self.validate(&u.types)?,
        })
    }
}

impl Validate<TsIntersectionType> for Analyzer<'_, '_> {
    type Output = ValidationResult<Intersection>;

    fn validate(&mut self, u: &TsIntersectionType) -> Self::Output {
        Ok(Intersection {
            span: u.span,
            types: self.validate(&u.types)?,
        })
    }
}

impl Validate<TsFnType> for Analyzer<'_, '_> {
    type Output = ValidationResult<ty::Function>;

    fn validate(&mut self, t: &TsFnType) -> Self::Output {
        Ok(ty::Function {})
    }
}

impl Validate<TsConstructorType> for Analyzer<'_, '_> {
    type Output = ValidationResult<ty::Constructor>;

    fn validate(&mut self, t: &TsConstructorType) -> Self::Output {
        Ok(ty::Constructor {})
    }
}

impl Validate<TsParenthesizedType> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, t: &TsParenthesizedType) -> Self::Output {
        t.type_ann.validate_with(self)
    }
}

impl Validate<TsType> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, ty: &TsType) -> Self::Output {
        let span = ty.span();

        Ok(match ty {
            TsType::TsThisType(this) => Type::This(*this),
            TsType::TsLitType(ty) => Type::Lit(*ty),
            TsType::TsKeywordType(ty) => Type::Keyword(*ty),
            TsType::TsTupleType(ty) => Type::Tuple(self.validate(ty)?),
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(u)) => {
                Type::Union(self.validate(u)?)
            }
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(i)) => {
                Type::Intersection(self.validate(i)?)
            }
            TsType::TsArrayType(arr) => Type::Array(self.validate(arr)?),
            TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(f)) => {
                Type::Function(self.validate(f)?)
            }
            TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsConstructorType(c)) => {
                Type::Constructor(self.validate(c)?)
            }
            TsType::TsTypeLit(lit) => Type::TypeLit(self.validate(lit)?),
            TsType::TsConditionalType(cond) => Type::Conditional(self.validate(&cond)?),
            TsType::TsMappedType(ty) => Type::Mapped(self.validate(ty)?),
            TsType::TsTypeOperator(ty) => Type::Operator(self.validate(ty)?),
            TsType::TsParenthesizedType(ty) => self.validate(ty)?,
            _ => unimplemented!("TsType: {:?}", ty),
        })
    }
}
