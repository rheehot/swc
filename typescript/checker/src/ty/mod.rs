use crate::{
    analyzer::Info,
    util::{EqIgnoreNameAndSpan, IntoCow},
    Exports,
};
use fxhash::FxHashMap;
use std::{borrow::Cow, mem::transmute, sync::Arc};
use swc_atoms::{js_word, JsWord};
use swc_common::{Fold, FromVariant, Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

mod convert;
pub mod merge;

pub trait TypeRefExt<'a>: Sized + Clone {
    fn into_type_ref(self) -> TypeRef<'a>;
    fn to_type_ref(&'a self) -> TypeRef<'a>;

    /// Returns generalized type if `self` is a literal type.
    fn generalize_lit(self) -> TypeRef<'a> {
        let ty = self.into_type_ref();

        match *ty.normalize() {
            Type::Lit(TsLitType { span, ref lit }) => {
                return Type::Keyword(TsKeywordType {
                    span,
                    kind: match *lit {
                        TsLit::Bool(Bool { .. }) => TsKeywordTypeKind::TsBooleanKeyword,
                        TsLit::Number(Number { .. }) => TsKeywordTypeKind::TsNumberKeyword,
                        TsLit::Str(Str { .. }) => TsKeywordTypeKind::TsStringKeyword,
                    },
                })
                .owned()
            }
            Type::Union(Union { ref types, .. }) => {
                let mut tys: Vec<Type> = Vec::with_capacity(types.len());

                for ty in types {
                    let ty = ty.clone().generalize_lit();
                    let dup = tys.iter().any(|t| t.eq_ignore_name_and_span(&ty));
                    if !dup {
                        tys.push(ty.into_owned());
                    }
                }

                return Type::union(tys).into_cow();
            }
            _ => {}
        }

        ty
    }

    fn to_static(&self) -> Type<'static> {
        self.clone().into_type_ref().into_owned().into_static()
    }

    fn cast<'b>(&'a self) -> TypeRef<'b>
    where
        'a: 'b,
    {
        // 'a lives longer than 'b, so this is ok
        unsafe { transmute::<TypeRef<'a>, TypeRef<'b>>(self.to_type_ref()) }
    }
}

impl<'a> TypeRefExt<'a> for TypeRef<'a> {
    #[inline]
    fn into_type_ref(self) -> TypeRef<'a> {
        self
    }

    fn to_type_ref(&'a self) -> TypeRef<'a> {
        match *self {
            Cow::Borrowed(b) => Cow::Borrowed(b),
            Cow::Owned(ref o) => Cow::Borrowed(o),
        }
    }
}

impl<'a> TypeRefExt<'a> for Type<'a> {
    #[inline]
    fn into_type_ref(self) -> TypeRef<'a> {
        Cow::Owned(self)
    }

    fn to_type_ref(&'a self) -> TypeRef<'a> {
        Cow::Borrowed(self)
    }
}

pub type TypeRef<'a> = Cow<'a, Type<'a>>;

#[derive(Debug, Fold, Clone, PartialEq, Spanned, FromVariant)]
pub enum Type<'a> {
    This(TsThisType),
    Lit(TsLitType),
    TypeLit(TypeLit<'a>),
    Keyword(TsKeywordType),
    Conditional(Conditional<'a>),
    Simple(Cow<'a, TsType>),
    Tuple(Tuple<'a>),
    Array(Array<'a>),
    Union(Union<'a>),
    Intersection(Intersection<'a>),
    Function(Function<'a>),
    Constructor(Constructor<'a>),
    Method(Method<'a>),

    Operator(Operator<'a>),

    Param(Param<'a>),
    EnumVariant(EnumVariant),

    Interface(Interface<'a>),
    Enum(Enum),

    Mapped(Mapped<'a>),

    /// export type A<B> = Foo<B>;
    Alias(Alias<'a>),
    Namespace(TsNamespaceDecl),
    Module(Module),

    Class(Class<'a>),
    /// Instance of the class.
    ///
    /// This variant is required ([TypeLit] is insufficient) because of codes
    /// like
    ///
    ///
    /// ```ts
    /// class A {
    ///     a: string;
    /// }
    ///
    /// class B {
    ///     a: string;
    ///     b: string;
    /// }
    /// ```
    ClassInstance(ClassInstance<'a>),

    /// Used for storing core types.
    ///
    /// Don't match on this directly. Instead, use `.normalize()`.
    Static(Static),

    Arc(#[fold(ignore)] Arc<Type<'static>>),
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Module {
    pub span: Span,
    #[fold(ignore)]
    pub exports: Exports<FxHashMap<JsWord, Arc<Type<'static>>>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Enum {
    pub span: Span,
    pub declare: bool,
    pub is_const: bool,
    pub id: Ident,
    pub members: Vec<EnumMember>,
    pub has_num: bool,
    pub has_str: bool,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct EnumMember {
    pub span: Span,
    pub id: TsEnumMemberId,
    pub val: TsLit,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Class<'a> {
    pub span: Span,
    pub is_abstract: bool,
    pub name: Option<JsWord>,
    pub super_class: Option<Box<TypeRef<'a>>>,
    pub body: Vec<ClassMember<'a>>,
    pub type_params: Option<TypeParamDecl<'a>>,
    // pub implements: Vec<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct ClassInstance<'a> {
    pub span: Span,
    pub cls: Class<'a>,
    pub type_args: Option<TypeParamInstantiation<'a>>,
    // pub implements: Vec<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned, FromVariant)]
pub enum ClassMember<'a> {
    Constructor(Constructor<'a>),
    Method(Method<'a>),
    ClassProp(ClassProp),
    TsIndexSignature(TsIndexSignature),
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Method<'a> {
    pub span: Span,
    pub key: PropName,
    pub is_static: bool,
    pub type_params: Option<TypeParamDecl<'a>>,
    pub params: Vec<Pat>,
    pub ret_ty: Box<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Mapped<'a> {
    pub span: Span,
    pub readonly: Option<TruePlusMinus>,
    pub optional: Option<TruePlusMinus>,
    pub type_param: TypeParam<'a>,
    pub ty: Option<Box<TypeRef<'a>>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Conditional<'a> {
    pub span: Span,
    pub check_type: Box<TypeRef<'a>>,
    pub extends_type: Box<TypeRef<'a>>,
    pub true_type: Box<TypeRef<'a>>,
    pub false_type: Box<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, Copy, PartialEq, Spanned)]
pub struct Static {
    pub span: Span,
    #[fold(ignore)]
    pub ty: &'static Type<'static>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Operator<'a> {
    pub span: Span,
    pub op: TsTypeOperatorOp,
    pub ty: Box<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Tuple<'a> {
    pub span: Span,
    pub types: Vec<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Alias<'a> {
    pub span: Span,
    pub type_params: Option<TypeParamDecl<'a>>,
    pub ty: Box<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Interface<'a> {
    pub span: Span,
    pub name: JsWord,
    pub type_params: Option<TypeParamDecl<'a>>,
    pub extends: Vec<TsExpr<'a>>,
    pub body: Vec<TypeElement<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct TypeLit<'a> {
    pub span: Span,
    pub members: Vec<TypeElement<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct TypeParamDecl<'a> {
    pub span: Span,
    pub params: Vec<TypeParam<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct TypeParam<'a> {
    pub span: Span,
    pub name: JsWord,

    pub constraint: Option<Box<TypeRef<'a>>>,
    pub default: Option<Box<TypeRef<'a>>>,
}

/// Typescript expression with type arguments
#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct TsExpr<'a> {
    pub span: Span,
    pub expr: TsEntityName,
    pub type_params: Option<TypeParamInstantiation<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct TypeParamInstantiation<'a> {
    pub span: Span,
    pub params: Vec<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned, FromVariant)]
pub enum TypeElement<'a> {
    Call(CallSignature<'a>),
    Constructor(ConstructorSignature<'a>),
    Property(PropertySignature<'a>),
    Method(MethodSignature<'a>),
    Index(IndexSignature<'a>),
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct CallSignature<'a> {
    pub span: Span,
    pub params: Vec<TsFnParam>,
    pub type_params: Option<TypeParamDecl<'a>>,
    pub ret_ty: Option<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct ConstructorSignature<'a> {
    pub span: Span,
    pub params: Vec<TsFnParam>,
    pub ret_ty: Option<TypeRef<'a>>,
    pub type_params: Option<TypeParamDecl<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct PropertySignature<'a> {
    pub span: Span,
    pub readonly: bool,
    pub key: Box<Expr>,
    pub computed: bool,
    pub optional: bool,
    pub params: Vec<TsFnParam>,
    pub type_ann: Option<TypeRef<'a>>,
    pub type_params: Option<TypeParamDecl<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct MethodSignature<'a> {
    pub span: Span,
    pub readonly: bool,
    pub key: Box<Expr>,
    pub computed: bool,
    pub optional: bool,
    pub params: Vec<TsFnParam>,
    pub ret_ty: Option<TypeRef<'a>>,
    pub type_params: Option<TypeParamDecl<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct IndexSignature<'a> {
    pub params: Vec<TsFnParam>,
    pub type_ann: Option<TypeRef<'a>>,

    pub readonly: bool,
    pub span: Span,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Array<'a> {
    pub span: Span,
    pub elem_type: Box<TypeRef<'a>>,
}

/// a | b
#[derive(Debug, Fold, Clone, Spanned)]
pub struct Union<'a> {
    pub span: Span,
    pub types: Vec<TypeRef<'a>>,
}

/// This impl is wrong.
///
/// This will be moved to EqIgnoreNameAndSpan in future. (I'll do this after
/// implementing a derive macro for EqIgnoreNameAndSpan)
impl PartialEq for Union<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.span != other.span || self.types.len() != other.types.len() {
            return false;
        }

        // A | B is equal to B | A
        //
        //
        // TODO: Make derive(EqIgnoreNameAndSpan) and move this to `impl
        // EqIgnoreNameAndSpan for Union`
        for ty in &self.types {
            if other
                .types
                .iter()
                .any(|oty| oty.eq_ignore_name_and_span(ty))
            {
                continue;
            }
            return false;
        }

        true
    }
}

/// a & b
#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Intersection<'a> {
    pub span: Span,
    pub types: Vec<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Param<'a> {
    pub span: Span,
    pub name: JsWord,
    pub constraint: Option<Box<TypeRef<'a>>>,
    pub default: Option<Box<TypeRef<'a>>>,
}

/// FooEnum.A
#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct EnumVariant {
    pub span: Span,
    pub enum_name: JsWord,
    pub name: JsWord,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Function<'a> {
    pub span: Span,
    pub type_params: Option<TypeParamDecl<'a>>,
    pub params: Vec<TsFnParam>,
    pub ret_ty: Box<TypeRef<'a>>,
}

#[derive(Debug, Fold, Clone, PartialEq, Spanned)]
pub struct Constructor<'a> {
    pub span: Span,
    pub type_params: Option<TypeParamDecl<'a>>,
    pub params: Vec<TsFnParam>,
    pub ret_ty: Option<Box<TypeRef<'a>>>,
}

impl Type<'_> {
    pub fn union<I: IntoIterator<Item = Self>>(iter: I) -> Self {
        let mut span = DUMMY_SP;

        let mut tys = vec![];

        for ty in iter {
            if span.is_dummy() {
                span = ty.span();
            }

            match ty {
                Type::Union(Union { types, .. }) => {
                    assert_ne!(types, vec![]);
                    tys.extend(types);
                }

                _ => tys.push(ty.into_cow()),
            }
        }

        match tys.len() {
            0 => unreachable!("Type::union() should not be called with an empty iterator"),
            1 => {
                let v = tys.into_iter().next().unwrap();
                v.into_owned()
            }
            _ => Type::Union(Union { span, types: tys }),
        }
    }

    pub fn contains_void(&self) -> bool {
        match *self.normalize() {
            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsVoidKeyword,
                ..
            }) => true,

            Type::Union(ref t) => t.types.iter().any(|t| t.contains_void()),

            _ => false,
        }
    }

    pub fn is_any(&self) -> bool {
        match *self.normalize() {
            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => true,

            Type::Union(ref t) => t.types.iter().any(|t| t.is_any()),

            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match *self.normalize() {
            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => true,

            Type::Union(ref t) => t.types.iter().any(|t| t.is_unknown()),

            _ => false,
        }
    }

    pub fn contains_undefined(&self) -> bool {
        match *self.normalize() {
            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsUndefinedKeyword,
                ..
            }) => true,

            Type::Union(ref t) => t.types.iter().any(|t| t.contains_undefined()),

            _ => false,
        }
    }
}

impl Type<'_> {
    pub fn is_keyword(&self, k: TsKeywordTypeKind) -> bool {
        match *self.normalize() {
            Type::Keyword(TsKeywordType { kind, .. }) if kind == k => true,
            _ => false,
        }
    }
    pub fn is_never(&self) -> bool {
        self.is_keyword(TsKeywordTypeKind::TsNeverKeyword)
    }

    pub const fn never<'any>(span: Span) -> Type<'any> {
        Type::Keyword(TsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsNeverKeyword,
        })
    }

    pub const fn undefined<'any>(span: Span) -> Type<'any> {
        Type::Keyword(TsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsUndefinedKeyword,
        })
    }

    pub const fn any<'any>(span: Span) -> Type<'any> {
        Type::Keyword(TsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsAnyKeyword,
        })
    }

    pub const fn unknown<'any>(span: Span) -> Type<'any> {
        Type::Keyword(TsKeywordType {
            span,
            kind: TsKeywordTypeKind::TsUnknownKeyword,
        })
    }
}

impl Type<'_> {
    pub fn respan(self, span: Span) -> Self {
        if self.span() == span {
            return self;
        }

        match self {
            Type::Operator(ty) => Type::Operator(Operator { span, ..ty }),

            Type::Mapped(ty) => Type::Mapped(Mapped { span, ..ty }),

            Type::Conditional(cond) => Type::Conditional(Conditional { span, ..cond }),

            Type::This(this) => Type::This(TsThisType { span, ..this }),

            Type::Lit(lit) => Type::Lit(TsLitType { span, ..lit }),

            Type::TypeLit(lit) => Type::TypeLit(TypeLit { span, ..lit }),

            Type::Keyword(kwd) => Type::Keyword(TsKeywordType { span, ..kwd }),

            Type::Array(arr) => Type::Array(Array { span, ..arr }),

            Type::Union(u) => Type::Union(Union { span, ..u }),

            Type::Intersection(u) => Type::Intersection(Intersection { span, ..u }),

            Type::Function(f) => Type::Function(Function { span, ..f }),

            Type::Constructor(c) => Type::Constructor(Constructor { span, ..c }),

            Type::Method(m) => Type::Method(Method { span, ..m }),

            Type::Enum(e) => Type::Enum(Enum { span, ..e }),

            Type::EnumVariant(e) => Type::EnumVariant(EnumVariant { span, ..e }),

            Type::Interface(e) => Type::Interface(Interface { span, ..e }),

            Type::Alias(a) => Type::Alias(Alias { span, ..a }),

            Type::Namespace(n) => Type::Namespace(TsNamespaceDecl { span, ..n }),

            Type::Module(m) => Type::Module(Module { span, ..m }),

            Type::Class(c) => Type::Class(Class { span, ..c }),

            Type::ClassInstance(c) => Type::ClassInstance(ClassInstance { span, ..c }),

            Type::Simple(ty) => Type::Simple(Cow::Owned(match *ty {
                TsType::TsTypeRef(ref t) => TsType::TsTypeRef(TsTypeRef { span, ..t.clone() }),
                ref ty => ty.clone(),
            })),

            Type::Param(p) => Type::Param(Param { span, ..p }),

            Type::Static(s) => Type::Static(Static { span, ..s }),

            Type::Tuple(ty) => Type::Tuple(Tuple { span, ..ty }),

            Type::Arc(arc) => Type::Arc(arc),
        }
    }
}

fn static_type(ty: Cow<Type>) -> TypeRef<'static> {
    ty.into_owned().into_static().into_cow()
}

fn map_types<'a, 'b, F>(types: Vec<TypeRef<'a>>, map: F) -> Vec<TypeRef<'b>>
where
    F: Fn(TypeRef<'a>) -> TypeRef<'b>,
{
    types.into_iter().map(map).collect()
}

impl Type<'_> {
    pub fn into_static(self) -> Type<'static> {
        match self {
            Type::Operator(ty) => Type::Operator(ty.into_static()),
            Type::Mapped(ty) => Type::Mapped(ty.into_static()),
            Type::Conditional(cond) => Type::Conditional(cond.into_static()),
            Type::This(this) => Type::This(this),
            Type::TypeLit(lit) => Type::TypeLit(lit.into_static()),
            Type::Lit(lit) => Type::Lit(lit),
            Type::Keyword(lit) => Type::Keyword(lit),
            Type::Simple(s) => Type::Simple(s.into_owned().into_cow()),
            Type::Array(Array { span, elem_type }) => Type::Array(Array {
                span,
                elem_type: box static_type(*elem_type),
            }),

            Type::Union(Union { span, types }) => Type::Union(Union {
                span,
                types: map_types(types, static_type),
            }),
            Type::Intersection(Intersection { span, types }) => Type::Intersection(Intersection {
                span,
                types: map_types(types, static_type),
            }),

            Type::Function(Function {
                span,
                type_params,
                params,
                ret_ty,
            }) => Type::Function(Function {
                span,
                type_params: type_params.map(|v| v.into_static()),
                params,
                ret_ty: box static_type(*ret_ty),
            }),

            Type::Constructor(Constructor {
                span,
                type_params,
                params,
                ret_ty,
            }) => Type::Constructor(Constructor {
                span,
                type_params: type_params.map(|v| v.into_static()),
                params,
                ret_ty: ret_ty.map(|ret_ty| box static_type(*ret_ty)),
            }),

            Type::Method(m) => Type::Method(m.into_static()),

            Type::Interface(i) => Type::Interface(i.into_static()),

            Type::Param(p) => Type::Param(p.into_static()),

            Type::Enum(e) => Type::Enum(e),
            Type::EnumVariant(e) => Type::EnumVariant(e),
            Type::Class(c) => Type::Class(c.into_static()),
            Type::ClassInstance(c) => Type::ClassInstance(c.into_static()),
            Type::Alias(a) => Type::Alias(a.into_static()),
            Type::Namespace(n) => Type::Namespace(n),
            Type::Module(m) => Type::Module(m),

            Type::Arc(ty) => Type::Arc(ty),

            Type::Static(s) => Type::Static(s),

            Type::Tuple(t) => Type::Tuple(t.into_static()),
        }
    }
}

impl<'a> Type<'a> {
    /// `Type::Static` is normalized.
    pub fn normalize<'s, 'c, 'd>(&'s self) -> &'c Type<'d>
    where
        'a: 'd,
        's: 'c,
    {
        match *self {
            Type::Static(Static { ty, .. }) => unsafe {
                // 'static lives longer than anything
                transmute::<&'static Type<'static>, &'c Type<'d>>(ty)
            },
            Type::Arc(ref s) => {
                //
                unsafe { transmute::<&'s Type<'static>, &'c Type<'d>>(s) }
            }
            _ => unsafe {
                // Shorten lifetimes
                transmute::<&'s Self, &'c Type<'d>>(self)
            },
        }
    }
}

impl Type<'_> {
    pub fn is_str(&self) -> bool {
        match self.normalize() {
            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsStringKeyword,
                ..
            })
            | Type::Lit(TsLitType {
                lit: TsLit::Str(..),
                ..
            }) => true,
            _ => false,
        }
    }
}

impl Type<'static> {
    /// Converts `Type<'static>` into `TypeRef<'a>`.
    pub fn owned<'a>(self) -> TypeRef<'a> {
        unsafe { transmute::<Cow<'_, Type<'static>>, TypeRef<'a>>(Cow::Owned(self)) }
    }

    /// Converts `Type<'static>` into `TypeRef<'a>`.
    #[inline]
    pub fn static_cast(&self) -> TypeRef {
        unsafe { transmute::<Cow<'_, Type<'static>>, TypeRef<'_>>(Cow::Borrowed(self)) }
    }
}

impl Interface<'_> {
    pub fn into_static(self) -> Interface<'static> {
        Interface {
            span: self.span,
            name: self.name,
            type_params: self.type_params.map(|v| v.into_static()),
            extends: self.extends.into_iter().map(|v| v.into_static()).collect(),
            body: self.body.into_iter().map(|v| v.into_static()).collect(),
        }
    }
}

impl TsExpr<'_> {
    pub fn into_static(self) -> TsExpr<'static> {
        TsExpr {
            span: self.span,
            expr: self.expr,
            type_params: self.type_params.map(|v| v.into_static()),
        }
    }
}

impl TypeElement<'static> {
    /// Converts `TypeTypeElement<'static>` into `TypeTypeElement<'a>`.
    #[inline]
    pub fn static_cast<'a>(self) -> TypeElement<'a> {
        unsafe { transmute::<TypeElement<'static>, TypeElement<'_>>(self) }
    }
}

impl TypeElement<'_> {
    pub fn into_static(self) -> TypeElement<'static> {
        match self {
            TypeElement::Call(call) => TypeElement::Call(call.into_static()),
            TypeElement::Constructor(c) => TypeElement::Constructor(c.into_static()),
            TypeElement::Index(i) => TypeElement::Index(i.into_static()),
            TypeElement::Method(m) => TypeElement::Method(m.into_static()),
            TypeElement::Property(p) => TypeElement::Property(p.into_static()),
        }
    }
}

impl TypeParamInstantiation<'_> {
    pub fn into_static(self) -> TypeParamInstantiation<'static> {
        TypeParamInstantiation {
            span: self.span,
            params: self.params.into_iter().map(static_type).collect(),
        }
    }
}

impl CallSignature<'_> {
    pub fn into_static(self) -> CallSignature<'static> {
        CallSignature {
            span: self.span,
            params: self.params,
            type_params: self.type_params.map(|v| v.into_static()),
            ret_ty: self.ret_ty.map(static_type),
        }
    }
}

impl ConstructorSignature<'_> {
    pub fn into_static(self) -> ConstructorSignature<'static> {
        ConstructorSignature {
            span: self.span,
            params: self.params,
            ret_ty: self.ret_ty.map(static_type),
            type_params: self.type_params.map(|v| v.into_static()),
        }
    }
}

impl IndexSignature<'_> {
    pub fn into_static(self) -> IndexSignature<'static> {
        IndexSignature {
            span: self.span,
            readonly: self.readonly,
            params: self.params,
            type_ann: self.type_ann.map(static_type),
        }
    }
}

impl MethodSignature<'_> {
    pub fn into_static(self) -> MethodSignature<'static> {
        MethodSignature {
            span: self.span,
            computed: self.computed,
            optional: self.optional,
            key: self.key,
            params: self.params,
            readonly: self.readonly,
            ret_ty: self.ret_ty.map(static_type),
            type_params: self.type_params.map(|v| v.into_static()),
        }
    }
}

impl PropertySignature<'_> {
    pub fn into_static(self) -> PropertySignature<'static> {
        PropertySignature {
            span: self.span,
            computed: self.computed,
            optional: self.optional,
            key: self.key,
            params: self.params,
            readonly: self.readonly,
            type_ann: self.type_ann.map(static_type),
            type_params: self.type_params.map(|v| v.into_static()),
        }
    }
}

impl TypeParam<'_> {
    pub fn into_static(self) -> TypeParam<'static> {
        TypeParam {
            span: self.span,
            name: self.name,
            constraint: self.constraint.map(|v| box static_type(*v)),
            default: self.default.map(|v| box static_type(*v)),
        }
    }
}

impl TypeParamDecl<'_> {
    pub fn into_static(self) -> TypeParamDecl<'static> {
        TypeParamDecl {
            span: self.span,
            params: self.params.into_iter().map(|v| v.into_static()).collect(),
        }
    }
}

impl TypeLit<'_> {
    pub fn into_static(self) -> TypeLit<'static> {
        TypeLit {
            span: self.span,
            members: self.members.into_iter().map(|v| v.into_static()).collect(),
        }
    }
}

impl Alias<'_> {
    pub fn into_static(self) -> Alias<'static> {
        Alias {
            span: self.span,
            type_params: self.type_params.map(|v| v.into_static()),
            ty: box static_type(*self.ty),
        }
    }
}

impl Param<'_> {
    pub fn into_static(self) -> Param<'static> {
        Param {
            span: self.span,
            name: self.name,
            constraint: self.constraint.map(|v| box static_type(*v)),
            default: self.default.map(|v| box static_type(*v)),
        }
    }
}

impl Tuple<'_> {
    pub fn into_static(self) -> Tuple<'static> {
        Tuple {
            span: self.span,
            types: self.types.into_iter().map(static_type).collect(),
        }
    }
}

impl Conditional<'_> {
    pub fn into_static(self) -> Conditional<'static> {
        Conditional {
            span: self.span,
            check_type: box static_type(*self.check_type),
            extends_type: box static_type(*self.extends_type),
            true_type: box static_type(*self.true_type),
            false_type: box static_type(*self.false_type),
        }
    }
}

impl Mapped<'_> {
    pub fn into_static(self) -> Mapped<'static> {
        Mapped {
            span: self.span,
            readonly: self.readonly,
            optional: self.optional,
            type_param: self.type_param.into_static(),
            ty: self.ty.map(|ty| box static_type(*ty)),
        }
    }
}

impl Operator<'_> {
    pub fn into_static(self) -> Operator<'static> {
        Operator {
            span: self.span,
            op: self.op,
            ty: box static_type(*self.ty),
        }
    }
}

impl Class<'_> {
    pub fn into_static(self) -> Class<'static> {
        Class {
            span: self.span,
            is_abstract: self.is_abstract,
            name: self.name,
            body: self.body.into_iter().map(|v| v.into_static()).collect(),
            type_params: self.type_params.map(|v| v.into_static()),
            super_class: self.super_class.map(|v| box static_type(*v)),
            // implements: map_types(self.implements, static_type),
        }
    }
}

impl ClassInstance<'_> {
    pub fn into_static(self) -> ClassInstance<'static> {
        ClassInstance {
            span: self.span,
            cls: self.cls.into_static(),
            type_args: self.type_args.map(|v| v.into_static()),
        }
    }
}

impl ClassMember<'_> {
    pub fn into_static(self) -> ClassMember<'static> {
        match self {
            ClassMember::Constructor(v) => ClassMember::Constructor(v.into_static()),
            ClassMember::Method(v) => ClassMember::Method(v.into_static()),
            ClassMember::ClassProp(v) => ClassMember::ClassProp(v),
            ClassMember::TsIndexSignature(v) => ClassMember::TsIndexSignature(v),
        }
    }
}

impl Constructor<'_> {
    pub fn into_static(self) -> Constructor<'static> {
        Constructor {
            span: self.span,
            params: self.params,
            type_params: self.type_params.map(|v| v.into_static()),
            ret_ty: self.ret_ty.map(|v| box Cow::Owned(v.to_static())),
        }
    }
}

impl TypeElement<'_> {
    pub fn key(&self) -> Option<&Expr> {
        static CONSTRUCTOR_EXPR: Expr =
            { Expr::Ident(Ident::new(js_word!("constructor"), DUMMY_SP)) };

        match *self {
            TypeElement::Call(..) => None,
            TypeElement::Constructor(..) => Some(&CONSTRUCTOR_EXPR),
            TypeElement::Index(..) => None,
            TypeElement::Method(ref el) => Some(&el.key),
            TypeElement::Property(ref el) => Some(&el.key),
        }
    }
}
