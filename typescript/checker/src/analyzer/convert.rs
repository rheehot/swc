use super::Analyzer;
use crate::{
    errors::Error,
    ty::{
        Alias, Array, CallSignature, Conditional, Constructor, ConstructorSignature, Enum,
        EnumMember, Function, IndexSignature, Interface, Intersection, Mapped, MethodSignature,
        Operator, PropertySignature, TsExpr, Tuple, Type, TypeElement, TypeLit, TypeParam,
        TypeParamDecl, TypeParamInstantiation, Union,
    },
    validator::Validate,
    ValidationResult,
};
use std::convert::TryFrom;
use swc_common::Spanned;
use swc_ecma_ast::*;

impl Validate<TsTypeParamDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeParamDecl>;

    fn validate(&mut self, decl: &TsTypeParamDecl) -> Self::Output {
        TypeParamDecl {
            span: decl.span,
            params: decl.params.into_iter().map(From::from).collect(),
        }
    }
}

impl Validate<TsTypeParam> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeParam>;

    fn validate(&mut self, p: &TsTypeParam) -> Self::Output {
        TypeParam {
            span: p.span,
            name: p.name.sym,
            constraint: p.constraint.map(|v| box v),
            default: p.default.map(|v| box v),
        }
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
            ty: self.validate(&d.type_ann)?,
            type_params: self.validate(&d.type_params)?,
        })
    }
}

impl Validate<TsInterfaceDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<Interface>;

    fn validate(&mut self, d: &TsInterfaceDecl) -> Self::Output {
        Ok(Interface {
            span: d.span,
            name: d.id.sym,
            type_params: d.type_params.map(From::from),
            extends: d.extends.into_iter().map(From::from).collect(),
            body: d.body.body.into_iter().map(From::from).collect(),
        })
    }
}

impl Validate<TsType> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, ty: &TsType) -> Self::Output {
        let span = ty.span();

        match ty {
            TsType::TsThisType(this) => this.into(),
            TsType::TsLitType(ty) => ty.into(),
            TsType::TsKeywordType(ty) => ty.into(),
            TsType::TsTupleType(ty) => Type::Tuple(ty.into()),
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(
                TsUnionType { span, types },
            )) => Union {
                span,
                types: types.into_iter().map(|v| v).collect(),
            }
            .into(),
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(
                TsIntersectionType { span, types },
            )) => Intersection {
                span,
                types: types.into_iter().map(|v| v).collect(),
            }
            .into(),
            TsType::TsArrayType(TsArrayType {
                span,
                box elem_type,
            }) => Type::Array(Array {
                span,
                elem_type: box elem_type,
            }),
            //TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
            //    span,
            //    params,
            //    type_params,
            //    type_ann,
            //})) => Type::Function(Function {
            //    span,
            //    params,
            //    type_params: type_params.map(From::from),
            //    ret_ty: box type_ann.type_ann,
            //}),
            //TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsConstructorType(
            //    TsConstructorType {
            //        span,
            //        params,
            //        type_params,
            //        type_ann,
            //    },
            //)) => Type::Constructor(Constructor { span, params }),
            TsType::TsTypeLit(lit) => Type::TypeLit(lit.into()),
            TsType::TsConditionalType(cond) => Type::Conditional(cond.into()),
            TsType::TsMappedType(ty) => Type::Mapped(ty.into()),
            TsType::TsTypeOperator(ty) => Type::Operator(ty.into()),
            TsType::TsParenthesizedType(TsParenthesizedType { type_ann, .. }) => type_ann.into(),
            _ => unimplemented!("TsType: {:?}", ty),
        }
    }
}

impl Validate<TsTypeLit> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeLit>;

    fn validate(&mut self, lit: &TsTypeLit) -> Self::Output {
        TypeLit {
            span: lit.span,
            members: lit.members.into_iter().map(From::from).collect(),
        }
    }
}

impl Validate<TsTypeElement> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeElement>;

    fn validate(&mut self, e: &TsTypeElement) -> Self::Output {
        match e {
            TsTypeElement::TsCallSignatureDecl(d) => TypeElement::Call(d.into()),
            TsTypeElement::TsConstructSignatureDecl(d) => TypeElement::Constructor(d.into()),
            TsTypeElement::TsIndexSignature(d) => TypeElement::Index(d.into()),
            TsTypeElement::TsMethodSignature(d) => TypeElement::Method(d.into()),
            TsTypeElement::TsPropertySignature(d) => TypeElement::Property(d.into()),
        }
    }
}

impl Validate<TsConstructSignatureDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<ConstructorSignature>;

    fn validate(&mut self, d: &TsConstructSignatureDecl) -> Self::Output {
        ConstructorSignature {
            span: d.span,
            params: d.params,
            type_params: d.type_params.map(From::from),
            ret_ty: d.type_ann.map(Type::from).map(Type::owned),
        }
    }
}

impl Validate<TsCallSignatureDecl> for Analyzer<'_, '_> {
    type Output = ValidationResult<CallSignature>;

    fn validate(&mut self, d: &TsCallSignatureDecl) -> Self::Output {
        CallSignature {
            span: d.span,
            params: d.params,
            type_params: d.type_params.map(From::from),
            ret_ty: d.type_ann.map(Type::from).map(Type::owned),
        }
    }
}

impl Validate<TsMethodSignature> for Analyzer<'_, '_> {
    type Output = ValidationResult<MethodSignature>;

    fn validate(&mut self, d: &TsMethodSignature) -> Self::Output {
        MethodSignature {
            span: d.span,
            readonly: d.readonly,
            key: d.key,
            computed: d.computed,
            optional: d.optional,
            params: d.params,
            ret_ty: d.type_ann.map(Type::from).map(Type::owned),
            type_params: d.type_params.map(From::from),
        }
    }
}

impl Validate<TsIndexSignature> for Analyzer<'_, '_> {
    type Output = ValidationResult<IndexSignature>;

    fn validate(&mut self, d: &TsIndexSignature) -> Self::Output {
        IndexSignature {
            span: d.span,
            params: self.validate(&d.params)?,
            readonly: d.readonly,
            type_ann: d.type_ann.map(|v| v),
        }
    }
}

impl Validate<TsPropertySignature> for Analyzer<'_, '_> {
    type Output = ValidationResult<PropertySignature>;

    fn validate(&mut self, d: &TsPropertySignature) -> Self::Output {
        PropertySignature {
            span: d.span,
            computed: d.computed,
            key: d.key,
            optional: d.optional,
            params: d.params,
            readonly: d.readonly,
            type_ann: d.type_ann.map(|v| v),
            type_params: d.type_params.map(From::from),
        }
    }
}

impl Validate<TsExprWithTypeArgs> for Analyzer<'_, '_> {
    type Output = ValidationResult<TsExpr>;

    fn validate(&mut self, e: &TsExprWithTypeArgs) -> Self::Output {
        TsExpr {
            span: e.span,
            expr: e.expr,
            type_params: e.type_args.map(From::from),
        }
    }
}

impl Validate<TsTypeParamInstantiation> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeParamInstantiation>;

    fn validate(&mut self, i: &TsTypeParamInstantiation) -> Self::Output {
        TypeParamInstantiation {
            span: i.span,
            params: i.params.into_iter().map(|v| v).collect(),
        }
    }
}

impl Validate<TsTupleType> for Analyzer<'_, '_> {
    type Output = ValidationResult<Tuple>;

    fn validate(&mut self, t: &TsTupleType) -> Self::Output {
        Tuple {
            span: t.span,
            types: t.elem_types.into_iter().map(|v| (*v)).collect(),
        }
    }
}

impl Validate<TsConditionalType> for Analyzer<'_, '_> {
    type Output = ValidationResult<Conditional>;

    fn validate(&mut self, t: &TsConditionalType) -> Self::Output {
        Conditional {
            span: t.span,
            check_type: box t.check_type,
            extends_type: box t.extends_type,
            true_type: box t.true_type,
            false_type: box t.false_type,
        }
    }
}

impl Validate<TsMappedType> for Analyzer<'_, '_> {
    type Output = ValidationResult<Mapped>;

    fn validate(&mut self, ty: &TsMappedType) -> Self::Output {
        Mapped {
            span: ty.span,
            readonly: ty.readonly,
            optional: ty.optional,
            type_param: ty.type_param.into(),
            ty: ty.type_ann.map(|v| box v),
        }
    }
}

impl Validate<TsTypeOperator> for Analyzer<'_, '_> {
    type Output = ValidationResult<Operator>;

    fn validate(&mut self, ty: &TsTypeOperator) -> Self::Output {
        Operator {
            span: ty.span,
            op: ty.op,
            ty: box ty.type_ann,
        }
    }
}

impl TryFrom<TsEnumDecl> for Type {
    type Error = Error;

    fn try_from(e: TsEnumDecl) -> Result<Self, Error> {
        Enum::try_from(e).map(From::from)
    }
}

impl TryFrom<TsEnumDecl> for Enum {
    type Error = Error;

    fn try_from(e: TsEnumDecl) -> Result<Self, Error> {
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

//impl Method {
//    pub fn into_static(self) -> Method<'static> {
//        Method {
//            span: self.span,
//            key: self.key,
//            is_static: self.is_static,
//            type_params: self.type_params.map(|v| v),
//            params: self.params,
//            ret_ty: box self.ret_ty,
//        }
//    }
//}
