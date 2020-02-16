use super::Type;
use crate::{
    ty,
    ty::{FnParam, TypeParamDecl},
};
use swc_common::Spanned;
use swc_ecma_ast::*;

impl From<Type> for TsType {
    fn from(t: Type) -> Self {
        match t {
            Type::This(t) => t.into(),
            Type::Lit(t) => t.into(),
            Type::Query(t) => t.into(),
            Type::Infer(t) => t.into(),
            Type::Import(t) => t.into(),
            Type::Predicate(t) => t.into(),
            Type::IndexedAccessType(t) => t.into(),
            Type::Ref(t) => t.into(),
            Type::TypeLit(t) => t.into(),
            Type::Keyword(t) => t.into(),
            Type::Conditional(t) => t.into(),
            Type::Tuple(t) => t.into(),
            Type::Array(t) => t.into(),
            Type::Union(t) => t.into(),
            Type::Intersection(t) => t.into(),
            Type::Function(t) => t.into(),
            Type::Constructor(t) => t.into(),
            Type::Method(t) => t.into(),
            Type::Operator(t) => t.into(),
            Type::Param(t) => t.into(),
            Type::EnumVariant(t) => t.into(),
            Type::Interface(t) => t.into(),
            Type::Enum(t) => t.into(),
            Type::Mapped(t) => t.into(),
            Type::Alias(t) => t.into(),
            Type::Namespace(t) => t.into(),
            Type::Module(t) => t.into(),
            Type::Class(t) => t.into(),
            Type::ClassInstance(t) => t.into(),
            Type::Static(t) => (*t.ty).clone().into(),
            Type::Arc(t) => (&t).clone().into(),
        }
    }
}

impl From<ty::QueryType> for TsType {
    fn from(t: ty::QueryType) -> Self {
        TsType::TsTypeQuery(TsTypeQuery {
            span: t.span,
            expr_name: t.expr.into(),
        })
    }
}

impl From<ty::InferType> for TsType {
    fn from(t: ty::InferType) -> Self {
        TsType::TsInferType(TsInferType {
            span: t.span,
            type_param: t.type_param.into(),
        })
    }
}

impl From<ty::ImportType> for TsType {
    fn from(t: ty::ImportType) -> Self {
        TsType::TsImportType(TsImportType {
            span: t.span,
            arg: t.arg,
            qualifier: t.qualifier,
            type_args: t.type_params.into(),
        })
    }
}

impl From<ty::Predicate> for TsType {
    fn from(t: ty::Predicate) -> Self {
        TsType::TsTypePredicate(TsTypePredicate {
            span: t.span,
            asserts: t.asserts,
            param_name: t.param_name,
            type_ann: t.ty.into(),
        })
    }
}

impl From<ty::IndexedAccessType> for TsType {
    fn from(t: ty::IndexedAccessType) -> Self {
        TsType::TsIndexedAccessType(TsIndexedAccessType {})
    }
}

impl From<ty::Ref> for TsType {
    fn from(t: ty::Ref) -> Self {
        TsType::TsTypeRef(TsTypeRef {
            span: t.span,
            type_name: t.type_name,
            type_params: t.type_args.map(From::from),
        })
    }
}

impl From<ty::TypeLit> for TsType {
    fn from(t: ty::TypeLit) -> Self {
        TsType::TsTypeLit(TsTypeLit {
            span: t.span,
            members: t.members.into_iter().map(From::from).collect(),
        })
    }
}

impl From<ty::Conditional> for TsType {
    fn from(t: ty::Conditional) -> Self {
        TsType::TsConditionalType(TsConditionalType {
            span: t.span,
            check_type: box (*t.check_type).into(),
            extends_type: box (*t.extends_type).into(),
            true_type: box (*t.true_type).into(),
            false_type: box (*t.false_type).into(),
        })
    }
}

impl From<ty::Tuple> for TsType {
    fn from(t: ty::Tuple) -> Self {
        TsType::TsTupleType(TsTupleType {
            span: t.span,
            elem_types: t.types.into_iter().map(|v| box v.into()).collect(),
        })
    }
}

impl From<ty::Array> for TsType {
    fn from(t: ty::Array) -> Self {
        TsType::TsArrayType(TsArrayType {
            span: t.span,
            elem_type: box (*t.elem_type).into(),
        })
    }
}

impl From<ty::Union> for TsType {
    fn from(t: ty::Union) -> Self {
        TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(TsUnionType {
            span: t.span,
            types: t.types.into_iter().map(From::from).collect(),
        }))
    }
}

impl From<ty::Intersection> for TsType {
    fn from(t: ty::Intersection) -> Self {
        TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(
            TsIntersectionType {
                span: t.span,
                types: t.types.into_iter().map(From::from).collect(),
            },
        ))
    }
}

impl From<ty::Function> for TsType {
    fn from(t: ty::Function) -> Self {
        TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
            type_params: t.type_params.map(From::from),
            type_ann: t.ret_ty.into(),
        }))
    }
}

impl From<ty::Constructor> for TsType {
    fn from(t: ty::Constructor) -> Self {
        TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsConstructorType(
            TsConstructorType {
                span: t.span,
                params: t.params.into_iter().map(From::from).collect(),
                type_params: t.type_params.map(From::from),
                type_ann: t.type_ann.into(),
            },
        ))
    }
}

impl From<ty::Method> for TsType {
    fn from(t: ty::Method) -> Self {
        TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
            type_params: t.type_params.map(From::from),
            type_ann: t.type_ann.into(),
        }))
    }
}

impl From<ty::TypeParamDecl> for TsTypeParamDecl {
    fn from(t: TypeParamDecl) -> Self {
        TsTypeParamDecl {
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
        }
    }
}

impl From<ty::Type> for TsTypeAnn {
    fn from(t: ty::Type) -> Self {
        TsTypeAnn {
            span: t.span(),
            type_ann: box t.into(),
        }
    }
}

impl From<Box<ty::Type>> for TsTypeAnn {
    fn from(t: Box<ty::Type>) -> Self {
        (*t).into()
    }
}

impl From<Box<ty::Type>> for Box<TsType> {
    fn from(t: Box<ty::Type>) -> Self {
        box (*t).into()
    }
}

impl From<ty::Operator> for TsTypeOperator {
    fn from(t: ty::Operator) -> Self {
        TsTypeOperator {
            span: t.span,
            op: t.op,
            type_ann: t.ty.into(),
        }
    }
}

impl From<ty::TypeParam> for TsTypeParam {
    fn from(t: ty::TypeParam) -> Self {
        TsTypeParam {
            span: t.span,
            name: t.name,
            constraint: t.constraint.into(),
            default: t.default.into(),
        }
    }
}

impl From<ty::Operator> for TsTypeOperator {
    fn from(t: ty::Operator) -> Self {}
}
