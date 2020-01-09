use super::Analyzer;
use crate::{
    analyzer::{pat::PatMode, props::prop_name_to_expr, util::ResultExt, Ctx, ScopeKind},
    builtin_types,
    errors::Error,
    ty,
    ty::{
        Array, ClassInstance, EnumVariant, FnParam, IndexSignature, Interface, Ref, Tuple, Type,
        TypeElement, TypeLit, TypeParamDecl, TypeParamInstantiation, Union,
    },
    util::{EqIgnoreNameAndSpan, EqIgnoreSpan, RemoveTypes},
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

mod bin;
mod call_new;
mod type_cast;
mod unary;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeOfMode {
    /// Used for l-values.
    ///
    /// This is used to allow
    ///
    /// ```ts
    /// type Num = { '0': string } | { [n: number]: number }
    /// declare var num: Num
    /// num[0] = 1
    /// num['0'] = 'ok'
    /// ```
    LValue,
    /// Use for r-values.
    RValue,
}

/// Delegates to Validate.
impl Visit<Expr> for Analyzer<'_, '_> {
    fn visit(&mut self, n: &Expr) {
        self.validate(n)
            .and_then(|ty| self.expand(n.span(), ty))
            .store(&mut self.info.errors);
    }
}

impl Validate<Expr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &Expr) -> Self::Output {
        self.validate_expr(e, TypeOfMode::RValue, None)
    }
}

prevent!(ParenExpr);

impl Validate<ParenExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &ParenExpr) -> Self::Output {
        self.validate(&e.expr)
    }
}

prevent!(AssignExpr);

impl Validate<AssignExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &AssignExpr) -> Self::Output {
        let ctx = Ctx {
            pat_mode: PatMode::Assign,
            ..self.ctx
        };
        self.with_ctx(ctx).with(|a| {
            let span = e.span();

            let any_span = match e.left {
                PatOrExpr::Pat(box Pat::Ident(ref i)) | PatOrExpr::Expr(box Expr::Ident(ref i)) => {
                    // Type is any if self.declaring contains ident
                    if a.scope.declaring.contains(&i.sym) {
                        Some(span)
                    } else {
                        None
                    }
                }

                _ => None,
            };

            e.left.visit_with(a);

            let mut errors = vec![];

            let rhs_ty = match e.right.validate_with(a) {
                Ok(rhs_ty) => {
                    a.check_rvalue(&rhs_ty);

                    Ok(rhs_ty)
                }
                Err(err) => {
                    errors.push(err);
                    Err(())
                }
            };

            a.info.errors.extend(errors);

            let rhs_ty = match rhs_ty {
                Ok(v) => v,
                Err(()) => Type::any(span),
            };

            if e.op == op!("=") {
                a.try_assign(span, &e.left, &rhs_ty);
            }

            if let Some(span) = any_span {
                return Ok(Type::any(span));
            }

            Ok(rhs_ty)
        })
    }
}

prevent!(UpdateExpr);

impl Validate<UpdateExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &UpdateExpr) -> Self::Output {
        let span = e.span;

        let ty = self
            .validate_expr(&e.arg, TypeOfMode::LValue, None)
            .and_then(|ty| match *ty.normalize() {
                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Lit(TsLitType {
                    lit: TsLit::Str(..),
                    ..
                })
                | Type::Array(..) => Err(Error::TS2356 { span: e.arg.span() }),

                _ => Ok(ty),
            })
            .store(&mut self.info.errors);

        if let Some(ty) = ty {
            if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword) {
                self.info.errors.push(Error::UpdateOpToSymbol {
                    span: e.arg.span(),
                    op: e.op,
                })
            }
        }

        Ok(Type::Keyword(TsKeywordType {
            kind: TsKeywordTypeKind::TsNumberKeyword,
            span,
        }))
    }
}

prevent!(SeqExpr);

impl Validate<SeqExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &SeqExpr) -> Self::Output {
        let SeqExpr { span, ref exprs } = *e;

        assert!(exprs.len() >= 1);

        let mut is_any = false;
        for e in exprs.iter() {
            match **e {
                Expr::Ident(ref i) => {
                    if self.scope.declaring.contains(&i.sym) {
                        is_any = true;
                    }
                }
                _ => {}
            }
            match self.validate(e) {
                Ok(..) => {}
                Err(Error::ReferencedInInit { .. }) => {
                    is_any = true;
                }
                Err(..) => {}
            }
        }
        if is_any {
            return Ok(Type::any(span));
        }

        return self.validate(&exprs.last().unwrap());
    }
}

impl Analyzer<'_, '_> {
    pub fn validate_expr(
        &mut self,
        e: &Expr,
        mode: TypeOfMode,
        type_args: Option<TypeParamInstantiation>,
    ) -> ValidationResult<Type> {
        let span = e.span();

        match e {
            // super() returns any
            Expr::Call(CallExpr {
                callee: ExprOrSuper::Super(..),
                ..
            }) => Ok(Type::any(span)),

            Expr::Bin(e) => self.validate(e),
            Expr::Cond(e) => self.validate(e),
            Expr::Seq(e) => self.validate(e),
            Expr::Update(e) => self.validate(e),
            Expr::New(e) => self.validate(e),
            Expr::Call(e) => self.validate(e),
            Expr::TsAs(e) => self.validate(e),
            Expr::TsTypeAssertion(e) => self.validate(e),
            Expr::Assign(e) => e.validate_with(self),
            Expr::Unary(e) => e.validate_with(self),

            Expr::This(ThisExpr { span }) => {
                let span = *span;
                if let Some(ty) = self.scope.this() {
                    return Ok(ty.into_owned());
                }
                return Ok(Type::from(TsThisType { span }));
            }

            Expr::Ident(ref i) => self.type_of_ident(i, mode),

            Expr::Array(ArrayLit { ref elems, .. }) => {
                let mut types: Vec<Type> = Vec::with_capacity(elems.len());

                for elem in elems {
                    let span = elem.span();
                    match elem {
                        Some(ExprOrSpread {
                            spread: None,
                            ref expr,
                        }) => {
                            let ty = self.validate(expr)?;
                            types.push(ty)
                        }
                        Some(ExprOrSpread {
                            spread: Some(..), ..
                        }) => unimplemented!("type of array spread"),
                        None => {
                            let ty = Type::undefined(span);
                            types.push(ty)
                        }
                    }
                }

                return Ok(Type::Tuple(Tuple { span, types }));
            }

            Expr::Lit(Lit::Bool(v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Bool(*v),
                }));
            }
            Expr::Lit(Lit::Str(ref v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Str(v.clone()),
                }));
            }
            Expr::Lit(Lit::Num(v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Number(*v),
                }));
            }
            Expr::Lit(Lit::Null(Null { span })) => {
                return Ok(Type::Keyword(TsKeywordType {
                    span: *span,
                    kind: TsKeywordTypeKind::TsNullKeyword,
                }));
            }
            Expr::Lit(Lit::Regex(..)) => {
                return Ok(Type::Ref(Ref {
                    span,
                    type_name: TsEntityName::Ident(Ident {
                        span,
                        sym: js_word!("RegExp"),
                        optional: false,
                        type_ann: None,
                    }),
                    type_params: None,
                }));
            }

            Expr::Paren(ParenExpr { ref expr, .. }) => self.validate(&expr),

            Expr::Tpl(ref t) => {
                // Check if tpl is constant. If it is, it's type is string literal.
                if t.exprs.is_empty() {
                    return Ok(Type::Lit(TsLitType {
                        span: t.span(),
                        lit: TsLit::Str(
                            t.quasis[0]
                                .cooked
                                .clone()
                                .unwrap_or_else(|| t.quasis[0].raw.clone()),
                        ),
                    }));
                }

                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                }));
            }

            Expr::TsNonNull(TsNonNullExpr { ref expr, .. }) => {
                Ok(expr.validate_with(self)?.remove_falsy())
            }

            Expr::Object(ObjectLit { span, ref props }) => {
                let mut members = Vec::with_capacity(props.len());
                let mut special_type = None;

                for prop in props.iter() {
                    match *prop {
                        PropOrSpread::Prop(ref prop) => {
                            members.push(prop.validate_with(self)?);
                        }
                        PropOrSpread::Spread(SpreadElement { ref expr, .. }) => {
                            match self.validate(&expr)? {
                                Type::TypeLit(TypeLit {
                                    members: spread_members,
                                    ..
                                }) => {
                                    members.extend(spread_members);
                                }

                                // Use last type on ...any or ...unknown
                                ty
                                @
                                Type::Keyword(TsKeywordType {
                                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                                    ..
                                })
                                | ty
                                @
                                Type::Keyword(TsKeywordType {
                                    kind: TsKeywordTypeKind::TsAnyKeyword,
                                    ..
                                }) => special_type = Some(ty),

                                ty => unimplemented!("spread with non-type-lit: {:#?}", ty),
                            }
                        }
                    }
                }

                if let Some(ty) = special_type {
                    return Ok(ty);
                }

                return Ok(Type::TypeLit(TypeLit {
                    span: *span,
                    members,
                }));
            }

            // https://github.com/Microsoft/TypeScript/issues/26959
            Expr::Yield(..) => return Ok(Type::any(span)),

            Expr::Await(AwaitExpr { .. }) => unimplemented!("typeof(AwaitExpr)"),

            Expr::Class(ClassExpr { ref class, .. }) => {
                return Ok(self.type_of_class(None, class)?.into());
            }

            Expr::Arrow(ref e) => return Ok(e.validate_with(self)?.into()),

            Expr::Fn(FnExpr { ref function, .. }) => {
                return Ok(function.validate_with(self)?.into());
            }

            Expr::Member(ref expr) => {
                return self.type_of_member_expr(expr, mode);
            }

            Expr::MetaProp(..) => unimplemented!("typeof(MetaProp)"),

            Expr::Invalid(ref i) => return Ok(Type::any(i.span())),

            _ => unimplemented!("typeof ({:?})", e),
        }
    }

    fn access_property(
        &mut self,
        span: Span,
        obj: Type,
        prop: &Expr,
        computed: bool,
        type_mode: TypeOfMode,
    ) -> ValidationResult {
        #[inline(never)]
        fn handle_type_elements(
            a: &mut Analyzer,
            span: Span,
            obj: &Type,
            prop: &Expr,
            computed: bool,
            type_mode: TypeOfMode,
            members: &[TypeElement],
        ) -> ValidationResult<Option<Type>> {
            let prop_ty = if computed {
                prop.validate_with(a)?.generalize_lit().into_owned()
            } else {
                match prop {
                    Expr::Ident(..) => Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        span,
                    }),
                    _ => unreachable!(),
                }
            };

            for el in members.iter() {
                match el {
                    TypeElement::Index(IndexSignature {
                        ref params,
                        ref type_ann,
                        ..
                    }) => {
                        if params.len() != 1 {
                            unimplemented!("Index signature with multiple parameters")
                        }

                        let index_ty = &params[0].ty;
                        if index_ty.eq_ignore_name_and_span(&prop_ty) {
                            if let Some(ref type_ann) = type_ann {
                                return Ok(Some(type_ann.clone()));
                            }
                            return Ok(Some(Type::any(span)));
                        }
                    }
                    _ => {}
                }

                if let Some(key) = el.key() {
                    let is_el_computed = match *el {
                        TypeElement::Property(ref p) => p.computed,
                        _ => false,
                    };
                    let is_eq = is_el_computed == computed
                        && match prop {
                            Expr::Ident(Ident { sym: ref value, .. })
                            | Expr::Lit(Lit::Str(Str { ref value, .. })) => match key {
                                Expr::Ident(Ident {
                                    sym: ref r_value, ..
                                })
                                | Expr::Lit(Lit::Str(Str {
                                    value: ref r_value, ..
                                })) => value == r_value,
                                _ => false,
                            },
                            _ => false,
                        };
                    if is_eq || key.eq_ignore_span(prop) {
                        match el {
                            TypeElement::Property(ref p) => {
                                if type_mode == TypeOfMode::LValue && p.readonly {
                                    return Err(Error::ReadOnly { span });
                                }

                                if let Some(ref type_ann) = p.type_ann {
                                    return Ok(Some(type_ann.clone()));
                                }

                                // TODO: no implicit any?
                                return Ok(Some(Type::any(span)));
                            }

                            TypeElement::Method(ref m) => {
                                //
                                return Ok(Some(Type::Function(ty::Function {
                                    span,
                                    type_params: m.type_params.clone(),
                                    params: m.params.clone(),
                                    ret_ty: box m.ret_ty.clone().unwrap_or_else(|| Type::any(span)),
                                })));
                            }

                            _ => unimplemented!("TypeElement {:?}", el),
                        }
                    }
                }
            }

            Ok(None)
        }

        if !self.is_builtin {
            debug_assert!(!span.is_dummy());
        }

        let obj: Type = self.expand(span, obj)?.generalize_lit().into_owned();

        match obj.normalize() {
            Type::Lit(..) => unreachable!(),

            Type::Enum(ref e) => {
                // TODO: Check if variant exists.
                macro_rules! ret {
                    ($sym:expr) => {{
                        // Computed values are not permitted in an enum with string valued members.
                        if e.is_const && type_mode == TypeOfMode::RValue {
                            for m in &e.members {
                                match m.id {
                                    TsEnumMemberId::Ident(Ident { ref sym, .. })
                                    | TsEnumMemberId::Str(Str { value: ref sym, .. }) => {
                                        if sym == $sym {
                                            return Ok(Type::Lit(TsLitType {
                                                span: m.span(),
                                                lit: m.val.clone().into(),
                                            }));
                                        }
                                    }
                                }
                            }
                        }

                        if e.is_const && computed {
                            return Err(Error::ConstEnumNonIndexAccess { span: prop.span() });
                        }

                        if e.is_const && type_mode == TypeOfMode::LValue {
                            return Err(Error::InvalidLValue { span: prop.span() });
                        }

                        debug_assert_ne!(span, prop.span());
                        return Ok(Type::EnumVariant(EnumVariant {
                            span: match type_mode {
                                TypeOfMode::LValue => prop.span(),
                                TypeOfMode::RValue => span,
                            },
                            enum_name: e.id.sym.clone(),
                            name: $sym.clone(),
                        }));
                    }};
                }
                match *prop {
                    Expr::Ident(Ident { ref sym, .. }) if !computed => {
                        ret!(sym);
                    }
                    Expr::Lit(Lit::Str(Str { value: ref sym, .. })) => {
                        ret!(sym);
                    }
                    Expr::Lit(Lit::Num(Number { value, .. })) => {
                        let idx = value.round() as usize;
                        if e.members.len() > idx {
                            return Ok(Type::Lit(TsLitType {
                                span,
                                lit: e.members[idx].val.clone(),
                            }));
                        }
                        return Ok(Type::Keyword(TsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        }));
                    }

                    _ => {
                        if e.is_const {
                            return Err(Error::ConstEnumNonIndexAccess { span: prop.span() });
                        }
                        return Err(Error::Unimplemented {
                            span,
                            msg: format!("access_property\nProp: {:?}", prop),
                        });
                    }
                }
            }

            // enum Foo { A }
            //
            // Foo.A.toString()
            Type::EnumVariant(EnumVariant {
                ref enum_name,
                ref name,
                span,
                ..
            }) => match self.find_type(enum_name) {
                Some(ref v) => match **v {
                    Type::Enum(ref e) => {
                        for v in e.members.iter() {
                            let new_obj_ty = Type::Lit(TsLitType {
                                span: *span,
                                lit: v.val.clone(),
                            });
                            return self
                                .access_property(*span, new_obj_ty, prop, computed, type_mode);
                        }
                        unreachable!("Enum {} does not have a variant named {}", enum_name, name);
                    }
                    _ => unreachable!("Enum named {} does not exist", enum_name),
                },
                _ => unreachable!("Enum named {} does not exist", enum_name),
            },

            Type::Class(ref c) => {
                for v in c.body.iter() {
                    match v {
                        ty::ClassMember::Property(ref class_prop) => {
                            match *class_prop.key {
                                Expr::Ident(ref i) => {
                                    if self.scope.declaring_prop.as_ref() == Some(&i.sym) {
                                        return Err(Error::ReferencedInInit { span });
                                    }
                                }
                                _ => {}
                            }
                            //
                            if (*class_prop.key).eq_ignore_span(&*prop) {
                                return Ok(match class_prop.value {
                                    Some(ref ty) => ty.clone(),
                                    None => Type::any(span),
                                });
                            }
                        }
                        ty::ClassMember::Method(ref mtd) => {
                            let mtd_key = prop_name_to_expr(&mtd.key);
                            if (*mtd_key).eq_ignore_span(&mtd_key) {
                                return Ok(Type::Method(mtd.clone()));
                            }
                        }
                        _ => unimplemented!("Non-property class member"),
                    }
                }
            }

            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => {
                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                }));
            }

            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Err(Error::Unknown { span: obj.span() }),

            Type::Keyword(TsKeywordType { kind, .. }) if !self.is_builtin => {
                let word = match kind {
                    TsKeywordTypeKind::TsStringKeyword => js_word!("String"),
                    TsKeywordTypeKind::TsNumberKeyword => js_word!("Number"),
                    TsKeywordTypeKind::TsBooleanKeyword => js_word!("Boolean"),
                    TsKeywordTypeKind::TsObjectKeyword => js_word!("Object"),
                    TsKeywordTypeKind::TsSymbolKeyword => js_word!("Symbol"),
                    _ => unimplemented!("access_property: obj: TSKeywordType {:?}", kind),
                };
                let interface = builtin_types::get_type(self.libs, span, &word)?;
                return self.access_property(span, interface, prop, computed, type_mode);
            }

            Type::Array(Array { elem_type, .. }) => {
                let array_ty = builtin_types::get_type(self.libs, span, &js_word!("Array"))
                    .expect("Array should be loaded");

                match prop.validate_with(self) {
                    Ok(ty) => match ty.normalize() {
                        Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                            ..
                        })
                        | Type::Lit(TsLitType {
                            lit: TsLit::Number(..),
                            ..
                        }) => return Ok(*elem_type.clone()),

                        _ => {}
                    },
                    _ => {}
                }

                return self.access_property(span, array_ty, prop, computed, type_mode);
            }

            Type::Interface(Interface { ref body, .. }) => {
                if let Some(v) =
                    handle_type_elements(self, span, &obj, prop, computed, type_mode, body)?
                {
                    return Ok(v);
                }

                // TODO: Check parent interfaces

                if computed {
                    let prop_ty = Some(prop.validate_with(self)?);
                    return Err(Error::NoSuchProperty {
                        span,
                        prop: Some(prop.clone()),
                        prop_ty,
                    });
                } else {
                    return Err(Error::NoSuchProperty {
                        span,
                        prop: Some(prop.clone()),
                        prop_ty: None,
                    });
                };
            }

            Type::TypeLit(TypeLit { ref members, .. }) => {
                if let Some(v) =
                    handle_type_elements(self, span, &obj, prop, computed, type_mode, members)?
                {
                    return Ok(v);
                }

                if computed {
                    let prop_ty = Some(prop.validate_with(self)?);
                    return Err(Error::NoSuchProperty {
                        span,
                        prop: Some(prop.clone()),
                        prop_ty,
                    });
                } else {
                    return Err(Error::NoSuchProperty {
                        span,
                        prop: Some(prop.clone()),
                        prop_ty: None,
                    });
                };
            }

            Type::Union(ty::Union { ref types, .. }) => {
                debug_assert!(types.len() >= 1);

                let mut tys = vec![];
                let mut errors = Vec::with_capacity(types.len());

                for ty in types {
                    match self.access_property(span, ty.clone(), prop, computed, type_mode) {
                        Ok(ty) => tys.push(ty),
                        Err(err) => errors.push(err),
                    }
                }

                if type_mode != TypeOfMode::LValue {
                    if !errors.is_empty() {
                        return Err(Error::UnionError { span, errors });
                    }
                } else {
                    // In l-value context, it's success if one of types matches it.
                    let is_err = errors.iter().any(|err| match *err {
                        Error::ReadOnly { .. } => true,
                        _ => false,
                    });
                    if tys.is_empty() || is_err {
                        assert_ne!(errors.len(), 0);
                        return Err(Error::UnionError { span, errors });
                    }
                }

                // TODO: Validate that the ty has same type instead of returning union.
                return Ok(Type::union(tys));
            }

            Type::Tuple(Tuple { ref types, .. }) => match *prop {
                Expr::Lit(Lit::Num(n)) => {
                    let v = n.value.round() as i64;
                    if v < 0 || types.len() <= v as usize {
                        return Err(Error::TupleIndexError {
                            span: n.span(),
                            index: v,
                            len: types.len() as u64,
                        });
                    }

                    return Ok(types[v as usize].clone());
                }
                _ => {
                    if types.is_empty() {
                        return Ok(Type::any(span));
                    }

                    //                    if types.len() == 1 {
                    //                        return Ok(Cow::Borrowed(&types[0]));
                    //                    }

                    return Ok(Type::Union(Union {
                        span,
                        types: types.clone(),
                    }));
                }
            },

            Type::ClassInstance(ClassInstance { ref cls, .. }) => {
                //
                for m in &cls.body {
                    //
                    match *m {
                        ty::ClassMember::Property(ref p) => {
                            // TODO: normalized string / ident
                            if (&*p.key).eq_ignore_name_and_span(&prop) {
                                if let Some(ref ty) = p.value {
                                    return Ok(ty.clone());
                                }

                                return Ok(Type::any(p.key.span()));
                            }
                        }
                        _ => {}
                    }
                }
            }

            Type::Module(ty::Module { ref exports, .. }) => match prop {
                Expr::Ident(Ident { ref sym, .. }) => {
                    if let Some(item) = exports.vars.get(sym) {
                        return Ok(item.clone().freeze());
                    }
                }
                _ => {}
            },

            Type::This(..) => {
                if let Some(this) = self.scope.this().map(|this| this.into_owned()) {
                    return self.access_property(span, this, prop, computed, type_mode);
                }
            }

            _ => {}
        }

        unimplemented!(
            "access_property(MemberExpr):\nObject: {:?}\nProp: {:?}",
            obj,
            prop
        );
    }

    pub fn type_of_ident(&mut self, i: &Ident, type_mode: TypeOfMode) -> ValidationResult {
        let span = i.span();

        match i.sym {
            js_word!("arguments") => return Ok(Type::any(span)),
            js_word!("Symbol") if !self.is_builtin => {
                return Ok(builtin_types::get_var(
                    self.libs,
                    i.span,
                    &js_word!("Symbol"),
                )?);
            }
            js_word!("undefined") => return Ok(Type::undefined(span)),
            js_word!("void") => return Ok(Type::any(span)),
            js_word!("eval") => match type_mode {
                TypeOfMode::LValue => return Err(Error::CannotAssignToNonVariable { span }),
                TypeOfMode::RValue => {
                    return Ok(Type::Function(ty::Function {
                        span,
                        params: vec![],
                        ret_ty: box Type::any(span),
                        type_params: None,
                    }));
                }
            },
            _ => {}
        }

        if i.sym == js_word!("require") {
            unreachable!("typeof(require('...'))");
        }

        if let Some(ty) = self.resolved_imports.get(&i.sym) {
            assert!(ty.is_arc());
            println!(
                "({}) type_of({}): resolved import",
                self.scope.depth(),
                i.sym
            );
            return Ok(ty.clone());
        }

        if let Some(ty) = self.find_type(&i.sym) {
            println!("({}) type_of({}): find_type", self.scope.depth(), i.sym);
            return Ok(ty.clone().respan(span));
        }

        // Check `declaring` before checking variables.
        if self.scope.declaring.contains(&i.sym) {
            println!(
                "({}) reference in initialization: {}",
                self.scope.depth(),
                i.sym
            );

            if self.ctx.allow_ref_declaring {
                return Ok(Type::any(span));
            } else {
                return Err(Error::ReferencedInInit { span });
            }
        }

        if let Some(ty) = self.find_var_type(&i.sym) {
            println!("({}) type_of({}): find_var_type", self.scope.depth(), i.sym);
            return Ok(ty.into_owned().respan(span));
        }

        if let Some(_var) = self.find_var(&i.sym) {
            // TODO: Infer type or use type hint to handle
            //
            // let id: (x: Foo) => Foo = x => x;
            //
            return Ok(Type::any(span));
        }

        if !self.is_builtin {
            if let Ok(ty) = builtin_types::get_var(self.libs, span, &i.sym) {
                return Ok(ty);
            }
        }

        if self.is_builtin {
            Ok(Type::Ref(Ref {
                span,
                type_name: TsEntityName::Ident(i.clone()),
                type_params: None,
            }))
        } else {
            Err(Error::UndefinedSymbol {
                span,
                sym: i.sym.clone(),
            })
        }
    }

    pub fn type_of_ts_entity_name(
        &mut self,
        span: Span,
        n: &TsEntityName,
        type_args: Option<TypeParamInstantiation>,
    ) -> ValidationResult {
        match *n {
            TsEntityName::Ident(ref i) => self.type_of_ident(i, TypeOfMode::RValue),
            TsEntityName::TsQualifiedName(ref qname) => {
                let obj_ty = self.type_of_ts_entity_name(span, &qname.left, None)?;

                self.access_property(
                    span,
                    obj_ty,
                    &Expr::Ident(qname.right.clone()),
                    false,
                    TypeOfMode::RValue,
                )
            }
        }
    }

    fn type_of_member_expr(
        &mut self,
        expr: &MemberExpr,
        type_mode: TypeOfMode,
    ) -> ValidationResult {
        let MemberExpr {
            ref obj,
            computed,
            ref prop,
            span,
            ..
        } = *expr;

        let mut errors = vec![];
        match *obj {
            ExprOrSuper::Expr(ref obj) => {
                let obj_ty = match obj.validate_with(self) {
                    Ok(ty) => ty,
                    Err(err) => {
                        // Recover error if possible.
                        if computed {
                            errors.push(err);
                            Type::any(span)
                        } else {
                            return Err(err);
                        }
                    }
                };

                if computed {
                    let ty = match self.access_property(span, obj_ty, prop, computed, type_mode) {
                        Ok(v) => Ok(v),
                        Err(err) => {
                            errors.push(err);

                            Err(())
                        }
                    };
                    if errors.is_empty() {
                        return Ok(ty.unwrap());
                    } else {
                        match prop.validate_with(self) {
                            Ok(..) => match ty {
                                Ok(ty) => {
                                    if errors.is_empty() {
                                        return Ok(ty);
                                    } else {
                                        return Err(Error::Errors { span, errors });
                                    }
                                }
                                Err(()) => return Err(Error::Errors { span, errors }),
                            },
                            Err(err) => errors.push(err),
                        }
                    }
                } else {
                    match self.access_property(span, obj_ty, prop, computed, type_mode) {
                        Ok(v) => return Ok(v),
                        Err(err) => {
                            errors.push(err);
                            return Err(Error::Errors { span, errors });
                        }
                    }
                }
            }
            _ => unimplemented!("type_of_member_expr(super.foo)"),
        }

        if errors.len() == 1 {
            return Err(errors.remove(0));
        }
        return Err(Error::Errors { span, errors });
    }
}

impl Validate<ArrowExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult<ty::Function>;

    fn validate(&mut self, f: &ArrowExpr) -> Self::Output {
        let declared_ret_ty = match f.return_type.validate_with(self) {
            Some(Ok(ty)) => Some(ty),
            Some(Err(err)) => {
                self.info.errors.push(err);
                Some(Type::any(f.span))
            }
            None => None,
        };
        let declared_ret_ty = match declared_ret_ty {
            Some(ty) => {
                let span = ty.span();
                Some(match ty {
                    Type::Class(cls) => Type::ClassInstance(ClassInstance {
                        span,
                        cls,
                        type_args: None,
                    }),
                    _ => ty,
                })
            }
            None => None,
        };

        let inferred_return_type = {
            match f.body {
                BlockStmtOrExpr::Expr(ref e) => Some(e.validate_with(self)?),
                BlockStmtOrExpr::BlockStmt(ref s) => self.visit_stmts_for_return(&s.stmts)?,
            }
        };
        if let Some(ref declared) = declared_ret_ty {
            let span = inferred_return_type.span();
            if let Some(ref inferred) = inferred_return_type {
                self.assign(declared, inferred, span)?;
            }
        }

        Ok(ty::Function {
            span: f.span,
            params: f.params.validate_with(self)?,
            type_params: try_opt!(f.type_params.validate_with(self)),
            ret_ty: box declared_ret_ty
                .unwrap_or_else(|| inferred_return_type.unwrap_or_else(|| Type::any(f.span))),
        })
    }
}

fn instantiate_class(ty: Type) -> Type {
    let span = ty.span();

    match *ty.normalize() {
        Type::Tuple(Tuple { ref types, span }) => Type::Tuple(Tuple {
            span,
            types: types
                .iter()
                .map(|ty| {
                    // TODO: Remove clone
                    instantiate_class(ty.clone())
                })
                .collect(),
        }),
        Type::Class(ref cls) => Type::ClassInstance(ClassInstance {
            // TODO
            span,

            // TODO; Remove clone
            cls: cls.clone(),

            // TODO
            type_args: None,
        }),
        _ => ty,
    }
}
