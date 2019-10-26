use super::{
    Alias, Array, CallSignature, Conditional, Constructor, ConstructorSignature, Function,
    IndexSignature, Interface, Intersection, Mapped, MethodSignature, Operator, PropertySignature,
    TsExpr, Tuple, Type, TypeElement, TypeLit, TypeParam, TypeParamDecl, TypeParamInstantiation,
    Union,
};
use crate::util::IntoCow;
use swc_ecma_ast::*;

impl From<TsTypeParamDecl> for TypeParamDecl<'_> {
    fn from(decl: TsTypeParamDecl) -> Self {
        TypeParamDecl {
            span: decl.span,
            params: decl.params.into_iter().map(From::from).collect(),
        }
    }
}

impl From<TsTypeParam> for TypeParam<'_> {
    fn from(p: TsTypeParam) -> Self {
        TypeParam {
            span: p.span,
            name: p.name.sym,
            constraint: p.constraint.map(|v| box v.into_cow()),
            default: p.default.map(|v| box v.into_cow()),
        }
    }
}

impl From<TsTypeAnn> for Type<'_> {
    #[inline]
    fn from(ann: TsTypeAnn) -> Self {
        ann.type_ann.into()
    }
}

impl<T> From<Box<T>> for Type<'_>
where
    T: Into<Self>,
{
    #[inline]
    fn from(ty: Box<T>) -> Self {
        (*ty).into()
    }
}

impl From<TsTypeAliasDecl> for Alias<'_> {
    fn from(d: TsTypeAliasDecl) -> Self {
        Alias {
            span: d.span,
            ty: box d.type_ann.into_cow(),
            type_params: d.type_params.map(From::from),
        }
    }
}

impl From<TsInterfaceDecl> for Interface<'_> {
    fn from(d: TsInterfaceDecl) -> Self {
        Interface {
            span: d.span,
            type_params: d.type_params.map(From::from),
            extends: d.extends.into_iter().map(From::from).collect(),
            body: d.body.body.into_iter().map(From::from).collect(),
        }
    }
}

impl From<TsInterfaceDecl> for Type<'_> {
    fn from(d: TsInterfaceDecl) -> Self {
        Type::Interface(d.into())
    }
}

impl From<TsType> for Type<'_> {
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
                elem_type: box elem_type.into_cow(),
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
                ret_ty: box type_ann.type_ann.into_cow(),
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

impl From<TsTypeAliasDecl> for Type<'_> {
    fn from(decl: TsTypeAliasDecl) -> Self {
        Type::Alias(decl.into())
    }
}

impl From<TsTypeLit> for TypeLit<'_> {
    fn from(lit: TsTypeLit) -> Self {
        TypeLit {
            span: lit.span,
            members: lit.members.into_iter().map(From::from).collect(),
        }
    }
}

impl From<TsTypeElement> for TypeElement<'_> {
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

impl From<TsConstructSignatureDecl> for ConstructorSignature<'_> {
    fn from(d: TsConstructSignatureDecl) -> Self {
        ConstructorSignature {
            span: d.span,
            params: d.params,
            type_params: d.type_params.map(From::from),
            ret_ty: d.type_ann.map(Type::from).map(Type::owned),
        }
    }
}

impl From<TsCallSignatureDecl> for CallSignature<'_> {
    fn from(d: TsCallSignatureDecl) -> Self {
        CallSignature {
            span: d.span,
            params: d.params,
            type_params: d.type_params.map(From::from),
            ret_ty: d.type_ann.map(Type::from).map(Type::owned),
        }
    }
}

impl From<TsMethodSignature> for MethodSignature<'_> {
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

impl From<TsIndexSignature> for IndexSignature<'_> {
    fn from(d: TsIndexSignature) -> Self {
        IndexSignature {
            span: d.span,
            params: d.params,
            readonly: d.readonly,
            type_ann: d.type_ann.map(|v| v.into_cow()),
        }
    }
}

impl From<TsPropertySignature> for PropertySignature<'_> {
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

impl From<TsExprWithTypeArgs> for TsExpr<'_> {
    fn from(e: TsExprWithTypeArgs) -> Self {
        TsExpr {
            span: e.span,
            expr: e.expr,
            type_params: e.type_params.map(From::from),
        }
    }
}

impl From<TsTypeParamInstantiation> for TypeParamInstantiation<'_> {
    fn from(i: TsTypeParamInstantiation) -> Self {
        TypeParamInstantiation {
            span: i.span,
            params: i.params.into_iter().map(|v| v.into_cow()).collect(),
        }
    }
}

impl From<TsTupleType> for Tuple<'_> {
    fn from(t: TsTupleType) -> Self {
        Tuple {
            span: t.span,
            types: t.elem_types.into_iter().map(|v| (*v).into_cow()).collect(),
        }
    }
}

impl From<TsConditionalType> for Conditional<'_> {
    fn from(t: TsConditionalType) -> Self {
        Conditional {
            span: t.span,
            check_type: box t.check_type.into_cow(),
            extends_type: box t.extends_type.into_cow(),
            true_type: box t.true_type.into_cow(),
            false_type: box t.false_type.into_cow(),
        }
    }
}

impl From<TsMappedType> for Mapped<'_> {
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

impl From<TsTypeOperator> for Operator<'_> {
    fn from(ty: TsTypeOperator) -> Self {
        Operator {
            span: ty.span,
            op: ty.op,
            ty: box ty.type_ann.into_cow(),
        }
    }
}

impl From<swc_ecma_ast::Constructor> for Constructor<'_> {
    fn from(c: swc_ecma_ast::Constructor) -> Self {
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
                    | PatOrTsParamProp::Pat(pat) => match pat {
                        Pat::Ident(v) => v.into(),
                        Pat::Array(v) => v.into(),
                        Pat::Rest(v) => v.into(),
                        Pat::Object(v) => v.into(),
                        _ => unreachable!("constructor with parameter {:?}", pat),
                    },
                })
                .collect(),
            type_params: None,
            ret_ty: None,
        }
    }
}
