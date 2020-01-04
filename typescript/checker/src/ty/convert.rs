use super::{
    Alias, Array, CallSignature, Conditional, Constructor, ConstructorSignature, Enum, EnumMember,
    Function, IndexSignature, Interface, Intersection, Mapped, Method, MethodSignature, Operator,
    PropertySignature, TsExpr, Tuple, Type, TypeElement, TypeLit, TypeParam, TypeParamDecl,
    TypeParamInstantiation, Union,
};
use crate::{errors::Error, util::IntoCow};
use std::convert::TryFrom;
use swc_common::Spanned;
use swc_ecma_ast::*;

impl From<TsTypeParamDecl> for TypeParamDecl {
    fn from(decl: TsTypeParamDecl) -> Self {
        TypeParamDecl {
            span: decl.span,
            params: decl.params.into_iter().map(From::from).collect(),
        }
    }
}

impl From<TsTypeParam> for TypeParam {
    fn from(p: TsTypeParam) -> Self {
        TypeParam {
            span: p.span,
            name: p.name.sym,
            constraint: p.constraint.map(|v| box v.into_cow()),
            default: p.default.map(|v| box v.into_cow()),
        }
    }
}

impl From<TsTypeAnn> for Type {
    #[inline]
    fn from(ann: TsTypeAnn) -> Self {
        ann.type_ann.into()
    }
}

impl<T> From<Box<T>> for Type
where
    T: Into<Self>,
{
    #[inline]
    fn from(ty: Box<T>) -> Self {
        (*ty).into()
    }
}

impl From<TsTypeAliasDecl> for Alias {
    fn from(d: TsTypeAliasDecl) -> Self {
        Alias {
            span: d.span,
            ty: box d.type_ann,
            type_params: d.type_params.map(From::from),
        }
    }
}

impl From<TsInterfaceDecl> for Interface {
    fn from(d: TsInterfaceDecl) -> Self {
        Interface {
            span: d.span,
            name: d.id.sym,
            type_params: d.type_params.map(From::from),
            extends: d.extends.into_iter().map(From::from).collect(),
            body: d.body.body.into_iter().map(From::from).collect(),
        }
    }
}

impl From<TsInterfaceDecl> for Type {
    fn from(d: TsInterfaceDecl) -> Self {
        Type::Interface(d.into())
    }
}

impl From<TsType> for Type {
    fn from(ty: TsType) -> Self {
        match ty {
            TsType::TsThisType(this) => this.into(),
            TsType::TsLitType(ty) => ty.into(),
            TsType::TsKeywordType(ty) => ty.into(),
            TsType::TsTupleType(ty) => Type::Tuple(ty.into()),
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(
                TsUnionType { span, types },
            )) => Union {
                span,
                types: types.into_iter().map(|v| v.into_cow()).collect(),
            }
            .into(),
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(
                TsIntersectionType { span, types },
            )) => Intersection {
                span,
                types: types.into_iter().map(|v| v.into_cow()).collect(),
            }
            .into(),
            TsType::TsArrayType(TsArrayType {
                span,
                box elem_type,
            }) => Type::Array(Array {
                span,
                elem_type: box elem_type,
            }),
            TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
                span,
                params,
                type_params,
                type_ann,
            })) => Type::Function(Function {
                span,
                params,
                type_params: type_params.map(From::from),
                ret_ty: box type_ann.type_ann,
            }),
            TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsConstructorType(
                TsConstructorType {
                    span,
                    params,
                    type_params,
                    type_ann,
                },
            )) => Type::Constructor(Constructor {
                span,
                params,
                type_params: type_params.map(From::from),
                ret_ty: Some(box type_ann.type_ann.into_cow()),
            }),
            TsType::TsTypeLit(lit) => Type::TypeLit(lit.into()),
            TsType::TsConditionalType(cond) => Type::Conditional(cond.into()),
            TsType::TsMappedType(ty) => Type::Mapped(ty.into()),
            TsType::TsTypeOperator(ty) => Type::Operator(ty.into()),
            TsType::TsParenthesizedType(TsParenthesizedType { type_ann, .. }) => type_ann.into(),
            _ => Type::Simple(ty.into_cow()),
        }
    }
}

impl From<TsTypeAliasDecl> for Type {
    fn from(decl: TsTypeAliasDecl) -> Self {
        Type::Alias(decl.into())
    }
}

impl From<TsTypeLit> for TypeLit {
    fn from(lit: TsTypeLit) -> Self {
        TypeLit {
            span: lit.span,
            members: lit.members.into_iter().map(From::from).collect(),
        }
    }
}

impl From<TsTypeElement> for TypeElement {
    fn from(e: TsTypeElement) -> Self {
        match e {
            TsTypeElement::TsCallSignatureDecl(d) => TypeElement::Call(d.into()),
            TsTypeElement::TsConstructSignatureDecl(d) => TypeElement::Constructor(d.into()),
            TsTypeElement::TsIndexSignature(d) => TypeElement::Index(d.into()),
            TsTypeElement::TsMethodSignature(d) => TypeElement::Method(d.into()),
            TsTypeElement::TsPropertySignature(d) => TypeElement::Property(d.into()),
        }
    }
}

impl From<TsConstructSignatureDecl> for ConstructorSignature {
    fn from(d: TsConstructSignatureDecl) -> Self {
        ConstructorSignature {
            span: d.span,
            params: d.params,
            type_params: d.type_params.map(From::from),
            ret_ty: d.type_ann.map(Type::from).map(Type::owned),
        }
    }
}

impl From<TsCallSignatureDecl> for CallSignature {
    fn from(d: TsCallSignatureDecl) -> Self {
        CallSignature {
            span: d.span,
            params: d.params,
            type_params: d.type_params.map(From::from),
            ret_ty: d.type_ann.map(Type::from).map(Type::owned),
        }
    }
}

impl From<TsMethodSignature> for MethodSignature {
    fn from(d: TsMethodSignature) -> Self {
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

impl From<TsIndexSignature> for IndexSignature {
    fn from(d: TsIndexSignature) -> Self {
        IndexSignature {
            span: d.span,
            params: d.params,
            readonly: d.readonly,
            type_ann: d.type_ann.map(|v| v.into_cow()),
        }
    }
}

impl From<TsPropertySignature> for PropertySignature {
    fn from(d: TsPropertySignature) -> Self {
        PropertySignature {
            span: d.span,
            computed: d.computed,
            key: d.key,
            optional: d.optional,
            params: d.params,
            readonly: d.readonly,
            type_ann: d.type_ann.map(|v| v.into_cow()),
            type_params: d.type_params.map(From::from),
        }
    }
}

impl From<TsExprWithTypeArgs> for TsExpr {
    fn from(e: TsExprWithTypeArgs) -> Self {
        TsExpr {
            span: e.span,
            expr: e.expr,
            type_params: e.type_args.map(From::from),
        }
    }
}

impl From<TsTypeParamInstantiation> for TypeParamInstantiation {
    fn from(i: TsTypeParamInstantiation) -> Self {
        TypeParamInstantiation {
            span: i.span,
            params: i.params.into_iter().map(|v| v.into_cow()).collect(),
        }
    }
}

impl From<TsTupleType> for Tuple {
    fn from(t: TsTupleType) -> Self {
        Tuple {
            span: t.span,
            types: t.elem_types.into_iter().map(|v| (*v).into_cow()).collect(),
        }
    }
}

impl From<TsConditionalType> for Conditional {
    fn from(t: TsConditionalType) -> Self {
        Conditional {
            span: t.span,
            check_type: box t.check_type,
            extends_type: box t.extends_type,
            true_type: box t.true_type,
            false_type: box t.false_type,
        }
    }
}

impl From<TsMappedType> for Mapped {
    fn from(ty: TsMappedType) -> Self {
        Mapped {
            span: ty.span,
            readonly: ty.readonly,
            optional: ty.optional,
            type_param: ty.type_param.into(),
            ty: ty.type_ann.map(|v| box v.into_cow()),
        }
    }
}

impl From<TsTypeOperator> for Operator {
    fn from(ty: TsTypeOperator) -> Self {
        Operator {
            span: ty.span,
            op: ty.op,
            ty: box ty.type_ann,
        }
    }
}

impl From<swc_ecma_ast::Constructor> for Constructor {
    fn from(c: swc_ecma_ast::Constructor) -> Self {
        fn from_pat(pat: Pat) -> TsFnParam {
            match pat {
                Pat::Ident(v) => v.into(),
                Pat::Array(v) => v.into(),
                Pat::Rest(v) => v.into(),
                Pat::Object(v) => v.into(),
                Pat::Assign(v) => from_pat(*v.left),
                _ => unreachable!("constructor with parameter {:?}", pat),
            }
        }

        Constructor {
            span: c.span,
            params: c
                .params
                .into_iter()
                .map(|v| match v {
                    PatOrTsParamProp::TsParamProp(TsParamProp {
                        param: TsParamPropParam::Ident(i),
                        ..
                    }) => TsFnParam::Ident(i),
                    PatOrTsParamProp::TsParamProp(TsParamProp {
                        param: TsParamPropParam::Assign(AssignPat { left: box pat, .. }),
                        ..
                    })
                    | PatOrTsParamProp::Pat(pat) => from_pat(pat),
                })
                .collect(),
            type_params: None,
            ret_ty: None,
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
//            type_params: self.type_params.map(|v| v.into_static()),
//            params: self.params,
//            ret_ty: box self.ret_ty.into_owned().into_static().owned(),
//        }
//    }
//}
