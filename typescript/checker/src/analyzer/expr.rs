use super::{
    control_flow::{Comparator, RemoveTypes},
    export::pat_to_ts_fn_param,
    Analyzer,
};
use crate::{
    builtin_types,
    errors::Error,
    ty::{
        self, Alias, Array, CallSignature, ConstructorSignature, EnumVariant, IndexSignature,
        Interface, Intersection, MethodSignature, PropertySignature, Tuple, Type, TypeElement,
        TypeLit, TypeParamDecl, TypeRef, TypeRefExt, Union,
    },
    util::{EqIgnoreNameAndSpan, EqIgnoreSpan, IntoCow},
};
use std::{borrow::Cow, iter::once};
use swc_atoms::js_word;
use swc_common::{Span, Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    pub(super) fn type_of<'e>(&'e self, expr: &Expr) -> Result<TypeRef<'e>, Error> {
        let span = expr.span();

        match *expr {
            Expr::This(ThisExpr { span }) => {
                if let Some(ref ty) = self.scope.this() {
                    return Ok(ty.static_cast());
                }
                return Ok(Cow::Owned(Type::from(TsThisType { span })));
            }

            Expr::Ident(Ident {
                sym: js_word!("undefined"),
                ..
            }) => return Ok(Type::undefined(span).into_cow()),

            Expr::Ident(ref i) => {
                if i.sym == js_word!("require") {
                    unreachable!("typeof(require('...'))");
                }

                if let Some(ty) = self.resolved_imports.get(&i.sym) {
                    println!(
                        "({}) type_of({}): resolved import",
                        self.scope.depth(),
                        i.sym
                    );
                    return Ok(ty.static_cast());
                }

                if let Some(ty) = self.find_type(&i.sym) {
                    println!("({}) type_of({}): find_type", self.scope.depth(), i.sym);
                    return Ok(ty.clone().respan(span).owned());
                }

                // Check `declaring` before checking variables.
                if self.declaring.contains(&i.sym) {
                    println!(
                        "({}) reference in initialization: {}",
                        self.scope.depth(),
                        i.sym
                    );

                    if self.allow_ref_declaring {
                        return Ok(Type::any(span).owned());
                    } else {
                        return Err(Error::ReferencedInInit { span });
                    }
                }

                if let Some(ty) = self.find_var_type(&i.sym) {
                    println!("({}) type_of({}): find_var_type", self.scope.depth(), i.sym);
                    return Ok(ty.clone().respan(span).owned());
                }

                if let Some(_var) = self.find_var(&i.sym) {
                    // TODO: Infer type or use type hint to handle
                    //
                    // let id: (x: Foo) => Foo = x => x;
                    //
                    return Ok(Type::any(span).into_cow());
                }

                if let Ok(ty) = builtin_types::get_var(self.libs, span, &i.sym) {
                    return Ok(ty.owned());
                }

                println!(
                    "({}) type_of(): undefined symbol: {}\n{:#?}",
                    self.scope.depth(),
                    i.sym,
                    self.scope
                );

                return Err(Error::UndefinedSymbol { span: i.span });
            }

            Expr::Array(ArrayLit { ref elems, .. }) => {
                let mut types: Vec<TypeRef> = Vec::with_capacity(elems.len());

                for elem in elems {
                    let span = elem.span();
                    match elem {
                        Some(ExprOrSpread {
                            spread: None,
                            ref expr,
                        }) => {
                            let ty = self.type_of(expr)?;
                            types.push(ty)
                        }
                        Some(ExprOrSpread {
                            spread: Some(..), ..
                        }) => unimplemented!("type of array spread"),
                        None => {
                            let ty = Type::undefined(span);
                            types.push(ty.into_cow())
                        }
                    }
                }

                return Ok(Type::Tuple(Tuple { span, types }).into_cow());
            }

            Expr::Lit(Lit::Bool(v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Bool(v),
                })
                .into_cow())
            }
            Expr::Lit(Lit::Str(ref v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Str(v.clone()),
                })
                .into_cow())
            }
            Expr::Lit(Lit::Num(v)) => {
                return Ok(Type::Lit(TsLitType {
                    span: v.span,
                    lit: TsLit::Number(v),
                })
                .into_cow())
            }
            Expr::Lit(Lit::Null(Null { span })) => {
                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsNullKeyword,
                })
                .into_cow())
            }
            Expr::Lit(Lit::Regex(..)) => {
                return Ok(TsType::TsTypeRef(TsTypeRef {
                    span,
                    type_name: TsEntityName::Ident(Ident {
                        span,
                        sym: js_word!("RegExp"),
                        optional: false,
                        type_ann: None,
                    }),
                    type_params: None,
                })
                .into_cow())
            }

            Expr::Paren(ParenExpr { ref expr, .. }) => return self.type_of(expr),

            Expr::Tpl(..) => {
                return Ok(Type::Keyword(TsKeywordType {
                    span,
                    kind: TsKeywordTypeKind::TsStringKeyword,
                })
                .into_cow())
            }

            Expr::Bin(BinExpr {
                span,
                op,
                ref left,
                ref right,
            }) => {
                let l_ty = self.type_of(&left)?;
                let r_ty = self.type_of(&right)?;
                macro_rules! no_unknown {
                    () => {{
                        no_unknown!(l_ty);
                        no_unknown!(r_ty);
                    }};
                    ($ty:expr) => {{
                        match *$ty {
                            Type::Keyword(TsKeywordType {
                                kind: TsKeywordTypeKind::TsUnknownKeyword,
                                ..
                            }) => {
                                return Err(Error::Unknown { span });
                            }
                            _ => {}
                        }
                    }};
                }

                match op {
                    op!(bin, "+") => {
                        no_unknown!();

                        let c = Comparator {
                            left: (&**left, &l_ty),
                            right: (&**right, &r_ty),
                        };

                        if let Some(()) = c.take(|(_, l_ty), (_, _)| match **l_ty {
                            Type::Keyword(TsKeywordType {
                                kind: TsKeywordTypeKind::TsUnknownKeyword,
                                ..
                            }) => Some(()),

                            _ => None,
                        }) {
                            return Err(Error::Unknown { span });
                        }

                        match *l_ty {
                            Type::Keyword(TsKeywordType {
                                kind: TsKeywordTypeKind::TsNumberKeyword,
                                ..
                            })
                            | Type::Lit(TsLitType {
                                lit: TsLit::Number(..),
                                ..
                            }) => match *r_ty {
                                Type::Keyword(TsKeywordType {
                                    kind: TsKeywordTypeKind::TsNumberKeyword,
                                    ..
                                })
                                | Type::Lit(TsLitType {
                                    lit: TsLit::Number(..),
                                    ..
                                }) => {
                                    return Ok(Type::Keyword(TsKeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsStringKeyword,
                                    })
                                    .owned())
                                }
                                _ => {}
                            },
                            _ => {}
                        }

                        if let Some(()) = c.take(|(_, l_ty), (_, _)| match **l_ty {
                            Type::Keyword(TsKeywordType {
                                kind: TsKeywordTypeKind::TsStringKeyword,
                                ..
                            })
                            | Type::Lit(TsLitType {
                                lit: TsLit::Str(..),
                                ..
                            }) => Some(()),

                            _ => None,
                        }) {
                            return Ok(Type::Keyword(TsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsStringKeyword,
                            })
                            .owned());
                        }

                        unimplemented!("type_of_bin(+)\nLeft: {:#?}\nRight: {:#?}", l_ty, r_ty)
                    }
                    op!("*") | op!("/") => {
                        no_unknown!();

                        return Ok(Type::Keyword(TsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        })
                        .owned());
                    }

                    op!(bin, "-")
                    | op!("<<")
                    | op!(">>")
                    | op!(">>>")
                    | op!("%")
                    | op!("|")
                    | op!("&")
                    | op!("^")
                    | op!("**") => {
                        no_unknown!();

                        return Ok(Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                            span,
                        })
                        .into_cow());
                    }

                    op!("===") | op!("!==") | op!("!=") | op!("==") => {
                        return Ok(Type::Keyword(TsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsBooleanKeyword,
                        })
                        .owned())
                    }

                    op!("<=") | op!("<") | op!(">=") | op!(">") | op!("in") | op!("instanceof") => {
                        no_unknown!();

                        return Ok(Type::Keyword(TsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsBooleanKeyword,
                        })
                        .owned());
                    }

                    op!("||") | op!("&&") => {
                        no_unknown!();

                        match l_ty.normalize() {
                            Type::Keyword(TsKeywordType {
                                kind: TsKeywordTypeKind::TsAnyKeyword,
                                ..
                            }) => return Ok(Type::any(span).owned()),

                            _ => {}
                        }

                        return Ok(r_ty);
                    }
                }
            }

            Expr::Unary(UnaryExpr { op, ref arg, .. }) => {
                match op {
                    op!("typeof") => {
                        return Ok(Type::Keyword(TsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsStringKeyword,
                        })
                        .into_cow())
                    }
                    _ => {}
                }
                let arg_ty = self.type_of(arg)?;
                match *arg_ty {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => return Err(Error::Unknown { span: arg.span() }),
                    _ => {}
                }

                match op {
                    op!("!") => return Ok(negate(self.type_of(arg)?.into_owned()).into_cow()),

                    op!("void") => return Ok(Type::undefined(span).owned()),

                    op!(unary, "-") | op!(unary, "+") => {
                        return Ok(Type::Keyword(TsKeywordType {
                            span,
                            kind: TsKeywordTypeKind::TsNumberKeyword,
                        })
                        .owned())
                    }

                    op => unimplemented!("type_of(Unary(op: {:?}))", op),
                }
            }

            Expr::TsAs(TsAsExpr { ref type_ann, .. }) => return Ok(type_ann.clone().into_cow()),
            Expr::TsTypeCast(TsTypeCastExpr { ref type_ann, .. }) => {
                return Ok(type_ann.type_ann.clone().into_cow())
            }

            Expr::TsNonNull(TsNonNullExpr { ref expr, .. }) => {
                return self.type_of(expr).map(|ty| {
                    // TODO: Optimize

                    ty.remove_falsy()
                });
            }

            Expr::Object(ObjectLit { span, ref props }) => {
                let mut members = Vec::with_capacity(props.len());
                let mut special_type = None;

                for prop in props.iter() {
                    match *prop {
                        PropOrSpread::Prop(ref prop) => {
                            members.push(self.type_of_prop(&prop));
                        }
                        PropOrSpread::Spread(SpreadElement { ref expr, .. }) => {
                            match self.type_of(&expr)?.into_owned() {
                                Type::TypeLit(TypeLit {
                                    members: spread_members,
                                    ..
                                }) => {
                                    members.extend(spread_members);
                                }

                                // Use last type on ...any or ...unknown
                                ty @ Type::Keyword(TsKeywordType {
                                    kind: TsKeywordTypeKind::TsUnknownKeyword,
                                    ..
                                })
                                | ty @ Type::Keyword(TsKeywordType {
                                    kind: TsKeywordTypeKind::TsAnyKeyword,
                                    ..
                                }) => special_type = Some(ty),

                                ty => unimplemented!("spread with non-type-lit: {:#?}", ty),
                            }
                        }
                    }
                }

                if let Some(ty) = special_type {
                    return Ok(ty.into_cow());
                }

                return Ok(Type::TypeLit(TypeLit { span, members }).into_cow());
            }

            // https://github.com/Microsoft/TypeScript/issues/26959
            Expr::Yield(..) => return Ok(Type::any(span).into_cow()),

            Expr::Update(..) => {
                return Ok(Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    span,
                })
                .into_cow())
            }

            Expr::Cond(CondExpr {
                ref cons, ref alt, ..
            }) => {
                let cons_ty = self.type_of(cons)?.into_owned();
                let alt_ty = self.type_of(alt)?.into_owned();

                return Ok(Type::union(once(cons_ty).chain(once(alt_ty))).into_cow());
            }

            Expr::New(NewExpr {
                ref callee,
                ref type_args,
                ref args,
                ..
            }) => {
                let callee_type = self.extract_call_new_expr_member(
                    callee,
                    ExtractKind::New,
                    args.as_ref().map(|v| &**v).unwrap_or_else(|| &[]),
                    type_args.as_ref(),
                )?;
                return Ok(callee_type);
            }

            Expr::Call(CallExpr {
                callee: ExprOrSuper::Expr(ref callee),
                ref args,
                ref type_args,
                ..
            }) => {
                let callee_type = self
                    .extract_call_new_expr_member(
                        callee,
                        ExtractKind::Call,
                        args,
                        type_args.as_ref(),
                    )
                    .map(|v| v)?;

                return Ok(callee_type);
            }

            // super() returns any
            Expr::Call(CallExpr {
                callee: ExprOrSuper::Super(..),
                ..
            }) => return Ok(Type::any(span).into_cow()),

            Expr::Seq(SeqExpr { ref exprs, .. }) => {
                assert!(exprs.len() >= 1);

                let mut is_any = false;
                for e in exprs.iter() {
                    match **e {
                        Expr::Ident(ref i) => {
                            if self.declaring.contains(&i.sym) {
                                is_any = true;
                            }
                        }
                        _ => {}
                    }
                    match self.type_of(e) {
                        Ok(..) => {}
                        Err(Error::ReferencedInInit { .. }) => {
                            is_any = true;
                        }
                        Err(..) => {}
                    }
                }
                if is_any {
                    return Ok(Type::any(span).owned());
                }

                return self.type_of(&exprs.last().unwrap());
            }

            Expr::Await(AwaitExpr { .. }) => unimplemented!("typeof(AwaitExpr)"),

            Expr::Class(ClassExpr { ref class, .. }) => {
                return Ok(self.type_of_class(class)?.owned())
            }

            Expr::Arrow(ref e) => return Ok(self.type_of_arrow_fn(e)?.owned()),

            Expr::Fn(FnExpr { ref function, .. }) => {
                return Ok(self.type_of_fn(&function)?.owned())
            }

            Expr::Member(ref expr) => {
                return self.type_of_member_expr(expr);
            }

            Expr::MetaProp(..) => unimplemented!("typeof(MetaProp)"),

            Expr::Assign(AssignExpr {
                left: PatOrExpr::Pat(box Pat::Ident(ref i)),
                ref right,
                ..
            }) => {
                if self.declaring.contains(&i.sym) {
                    return Ok(Type::any(span).owned());
                }

                return self.type_of(right);
            }

            Expr::Assign(AssignExpr {
                left: PatOrExpr::Expr(ref left),
                ref right,
                ..
            }) => {
                match **left {
                    Expr::Ident(ref i) => {
                        if self.declaring.contains(&i.sym) {
                            return Ok(Type::any(span).owned());
                        }
                    }
                    _ => {}
                }

                return self.type_of(right);
            }

            Expr::Assign(AssignExpr { ref right, .. }) => return self.type_of(right),

            Expr::TsTypeAssertion(TsTypeAssertion { ref type_ann, .. }) => {
                return Ok(Type::from(type_ann.clone()).owned())
            }

            _ => unimplemented!("typeof ({:#?})", expr),
        }
    }

    fn type_of_member_expr(&self, expr: &MemberExpr) -> Result<TypeRef, Error> {
        let MemberExpr {
            ref obj,
            computed,
            ref prop,
            span,
            ..
        } = *expr;

        match *obj {
            ExprOrSuper::Expr(ref obj) => {
                let obj_ty = self.type_of(obj)?;

                return self.access_property(span, obj_ty, prop, computed);
            }
            _ => unimplemented!("type_of_member_expr(super.foo)"),
        }
    }

    fn type_of_prop(&self, prop: &Prop) -> TypeElement {
        let span = prop.span();

        match *prop {
            Prop::Shorthand(..) => PropertySignature {
                span: prop.span(),
                key: prop_key_to_expr(&prop),
                params: Default::default(),
                optional: false,
                readonly: false,
                computed: false,
                type_ann: Default::default(),
                type_params: Default::default(),
            }
            .into(),

            Prop::KeyValue(..) => PropertySignature {
                span: prop.span(),
                key: prop_key_to_expr(&prop),
                params: Default::default(),
                optional: false,
                readonly: false,
                computed: false,
                type_ann: Default::default(),
                type_params: Default::default(),
            }
            .into(),

            Prop::Assign(ref p) => unimplemented!("type_of_prop(AssignProperty): {:#?}", p),
            Prop::Getter(ref p) => unimplemented!("type_of_prop(GetterProperty): {:#?}", p),
            Prop::Setter(ref p) => unimplemented!("type_of_prop(SetterProperty): {:#?}", p),

            Prop::Method(ref p) => MethodSignature {
                span,
                readonly: false,
                key: prop_key_to_expr(&prop),
                computed: false,
                optional: false,
                params: p
                    .function
                    .params
                    .iter()
                    .cloned()
                    .map(pat_to_ts_fn_param)
                    .collect(),
                ret_ty: p.function.return_type.clone().map(|v| v.into_cow()),
                type_params: p.function.type_params.clone().map(|v| v.into()),
            }
            .into(),
        }
    }

    fn access_property<'a>(
        &self,
        span: Span,
        obj: TypeRef,
        prop: &Expr,
        computed: bool,
    ) -> Result<TypeRef<'a>, Error> {
        let obj = obj.generalize_lit();
        match *obj.normalize() {
            Type::Lit(..) => unreachable!(),

            Type::Enum(ref e) => {
                // TODO: Check if variant exists.
                match *prop {
                    Expr::Ident(ref v) if !computed => {
                        return Ok(Cow::Owned(Type::EnumVariant(EnumVariant {
                            span,
                            enum_name: e.id.sym.clone(),
                            name: v.sym.clone(),
                        })))
                    }
                    _ => {}
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
                        for (i, v) in e.members.iter().enumerate() {
                            let new_obj = v.init.clone().unwrap_or_else(|| {
                                box Expr::Lit(Lit::Num(Number {
                                    span,
                                    value: i as f64,
                                }))
                            });
                            let new_obj_ty = self.type_of(&new_obj)?;
                            return self.access_property(span, new_obj_ty, prop, computed);
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
                        ClassMember::ClassProp(ref class_prop) => {
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
                                return Ok(
                                    match class_prop.type_ann.clone().map(|v| v.into_cow()) {
                                        Some(ty) => ty,
                                        None => Type::any(span).owned(),
                                    },
                                );
                            }
                        }
                        ClassMember::Method(ref mtd) => {
                            let mtd_key = prop_name_to_expr(&mtd.key);
                            if (*mtd_key).eq_ignore_span(&mtd_key) {
                                return Ok(self.type_of_fn(&mtd.function)?.owned());
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
                })
                .owned())
            }

            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Err(Error::Unknown { span: obj.span() }),

            Type::Array(Array { .. }) => {
                let array_ty = builtin_types::get_type(self.libs, span, &js_word!("Array"))
                    .expect("Array should be loaded")
                    .owned();

                // TODO: Array<elem_type>

                return self.access_property(span, array_ty, prop, computed);
            }

            Type::Interface(Interface { ref body, .. }) => {
                let prop_ty = self.type_of(prop)?.generalize_lit();

                for el in body.iter() {
                    match el {
                        TypeElement::Index(IndexSignature {
                            ref params,
                            ref type_ann,
                            ..
                        }) => {
                            if params.len() != 1 {
                                unimplemented!("Index signature with multiple parameters")
                            }
                            match params[0] {
                                TsFnParam::Ident(ref i) => {
                                    assert!(i.type_ann.is_some());

                                    let index_ty = Type::from(i.type_ann.as_ref().unwrap().clone());
                                    if index_ty.eq_ignore_name_and_span(&*prop_ty) {
                                        if let Some(ref type_ann) = type_ann {
                                            return Ok(type_ann.to_static().owned());
                                        }
                                        return Ok(Type::any(span).owned());
                                    }
                                }

                                _ => unimplemented!("TsFnParam other than index in IndexSignature"),
                            }
                        }
                        _ => {}
                    }

                    if let Some(key) = el.key() {
                        if key.eq_ignore_span(prop) {
                            match el {
                                TypeElement::Property(ref p) => {
                                    if let Some(ref type_ann) = p.type_ann {
                                        return Ok(type_ann.to_static().owned());
                                    }

                                    // TODO: no implicit any?
                                    return Ok(Type::any(span).owned());
                                }

                                _ => {}
                            }
                        }
                    }
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

    /// In almost case, this method returns `Ok`.
    pub(super) fn validate_type_of_class(&mut self, c: &Class) -> Result<Type<'static>, Error> {
        for m in c.body.iter() {
            let span = m.span();

            match *m {
                ClassMember::ClassProp(ref prop) => match prop.type_ann {
                    Some(ref ty) => {
                        let ty = Type::from(ty.clone());
                        if ty.is_any() || ty.is_unknown() {
                        } else {
                            if prop.value.is_none() {
                                self.info
                                    .errors
                                    .push(Error::ClassPropertyInitRequired { span })
                            }
                        }
                    }
                    None => {}
                },
                _ => {}
            }
        }

        self.type_of_class(c)
    }

    fn type_of_class(&self, c: &Class) -> Result<Type<'static>, Error> {
        // let mut type_props = vec![];
        // for member in &c.body {
        //     let span = member.span();
        //     let any = any(span);

        //     match member {
        //         ClassMember::ClassProp(ref p) => {
        //             let ty = match p.type_ann.as_ref().map(|ty|
        // Type::from(&*ty.type_ann)) {                 Some(ty) => ty,
        //                 None => match p.value {
        //                     Some(ref e) => self.type_of(&e)?,
        //                     None => any,
        //                 },
        //             };

        //
        // type_props.push(TypeElement::TsPropertySignature(TsPropertySignature {
        //                 span,
        //                 key: p.key.clone(),
        //                 optional: p.is_optional,
        //                 readonly: p.readonly,
        //                 init: p.value.clone(),
        //                 type_ann: Some(TsTypeAnn {
        //                     span: ty.span(),
        //                     type_ann: box ty.into_owned(),
        //                 }),

        //                 // TODO:
        //                 computed: false,

        //                 // TODO:
        //                 params: Default::default(),

        //                 // TODO:
        //                 type_params: Default::default(),
        //             }));
        //         }

        //         // TODO:
        //         ClassMember::Constructor(ref c) => {
        //             type_props.push(TypeElement::TsConstructSignatureDecl(
        //                 TsConstructSignatureDecl {
        //                     span,

        //                     // TODO:
        //                     type_ann: None,

        //                     params: c
        //                         .params
        //                         .iter()
        //                         .map(|param| match *param {
        //                             PatOrTsParamProp::Pat(ref pat) => {
        //                                 pat_to_ts_fn_param(pat.clone())
        //                             }
        //                             PatOrTsParamProp::TsParamProp(ref prop) => match
        // prop.param {
        // TsParamPropParam::Ident(ref i) => {
        // TsFnParam::Ident(i.clone())                                 }
        //                                 TsParamPropParam::Assign(AssignPat {
        //                                     ref left, ..
        //                                 }) => pat_to_ts_fn_param(*left.clone()),
        //                             },
        //                         })
        //                         .collect(),

        //                     // TODO:
        //                     type_params: Default::default(),
        //                 },
        //             ));
        //         }

        //         // TODO:
        //         ClassMember::Method(..) => {}

        //         // TODO:
        //         ClassMember::TsIndexSignature(..) => {}

        //         ClassMember::PrivateMethod(..) | ClassMember::PrivateProp(..) => {}
        //     }
        // }

        // Ok(TsType::TsTypeLit(TsTypeLit {
        //     span: c.span(),
        //     members: type_props,
        // })
        // .into())
        Ok(Type::Class(c.clone()))
    }

    pub(super) fn infer_return_type(
        &self,
        base_span: Span,
    ) -> Result<Option<Type<'static>>, Error> {
        let types = { ::std::mem::replace(&mut *self.inferred_return_types.borrow_mut(), vec![]) };

        // TODO: Handle recursive function.

        let mut tys = Vec::with_capacity(types.len());
        for ty in types {
            tys.push(ty);
        }

        match tys.len() {
            0 => Ok(None),
            _ => Ok(Some(Type::union(tys))),
        }
    }

    pub(super) fn type_of_arrow_fn(&self, f: &ArrowExpr) -> Result<Type<'static>, Error> {
        let declared_ret_ty = f.return_type.as_ref().map(|ret_ty| {
            self.expand_type(f.span, Cow::Owned(Type::from(ret_ty.type_ann.clone())))
                .map(|v| v.to_static())
        });
        let declared_ret_ty = match declared_ret_ty {
            Some(Ok(ty)) => Some(ty),
            Some(Err(err)) => return Err(err),
            None => None,
        };

        let inferred_return_type = self.infer_return_type(f.span)?;
        if let Some(ref declared) = declared_ret_ty {
            let span = inferred_return_type.span();
            if let Some(ref inffered) = inferred_return_type {
                self.assign(declared, inffered, span)?;
            }
        }

        Ok(ty::Function {
            span: f.span,
            params: f.params.iter().cloned().map(pat_to_ts_fn_param).collect(),
            type_params: f.type_params.clone().map(From::from),
            ret_ty: box declared_ret_ty
                .unwrap_or_else(|| {
                    inferred_return_type
                        .map(|v| v.to_static())
                        .unwrap_or_else(|| Type::any(f.span))
                })
                .owned(),
        }
        .into())
    }

    pub(super) fn type_of_fn(&self, f: &Function) -> Result<Type<'static>, Error> {
        let declared_ret_ty = f.return_type.as_ref().map(|ret_ty| {
            self.expand_type(f.span, Cow::Owned(Type::from(ret_ty.type_ann.clone())))
                .map(|v| v.to_static())
        });
        let declared_ret_ty = match declared_ret_ty {
            Some(Ok(ty)) => Some(ty),
            Some(Err(err)) => return Err(err),
            None => None,
        };

        let inferred_return_type = f
            .body
            .as_ref()
            .map(|body| self.infer_return_type(body.span));
        let inferred_return_type = match inferred_return_type {
            Some(Ok(Some(inferred_return_type))) => {
                if let Some(ref declared) = declared_ret_ty {
                    let span = inferred_return_type.span();

                    self.assign(&declared, &inferred_return_type, span)?;
                }

                inferred_return_type
            }
            Some(Ok(None)) => {
                let mut span = f.span;

                if let Some(ref declared) = declared_ret_ty {
                    span = declared.span();

                    match *declared.normalize() {
                        Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsUnknownKeyword,
                            ..
                        }) => return Err(Error::ReturnRequired { span }),
                        _ => {}
                    }
                }

                Type::any(span)
            }
            Some(Err(err)) => return Err(err),
            None => Type::any(f.span),
        };

        Ok(ty::Function {
            span: f.span,
            params: f.params.iter().cloned().map(pat_to_ts_fn_param).collect(),
            type_params: f.type_params.clone().map(From::from),
            ret_ty: box declared_ret_ty
                .map(|ty| ty.to_static().owned())
                .unwrap_or(inferred_return_type.to_static().owned()),
        }
        .into())
    }

    fn extract_call_new_expr_member<'e>(
        &'e self,
        callee: &Expr,
        kind: ExtractKind,
        args: &[ExprOrSpread],
        type_args: Option<&TsTypeParamInstantiation>,
    ) -> Result<TypeRef<'e>, Error> {
        let span = callee.span();

        macro_rules! search_members_for_prop {
            ($members:expr, $prop:expr) => {{
                // Candidates of the method call.
                //
                // 4 is just an unsientific guess
                let mut candidates = Vec::with_capacity(4);

                for m in $members {
                    match m {
                        TypeElement::Method(ref m) if kind == ExtractKind::Call => {
                            // We are only interested on methods named `prop`
                            if $prop.eq_ignore_span(&m.key) {
                                candidates.push(m.clone());
                            }
                        }

                        _ => {}
                    }
                }

                match candidates.len() {
                    0 => {}
                    1 => {
                        let MethodSignature { ret_ty, .. } = candidates.into_iter().next().unwrap();

                        return Ok(ret_ty.unwrap_or_else(|| Type::any(span).owned()));
                    }
                    _ => {
                        //
                        for c in candidates {
                            if c.params.len() == args.len() {
                                return Ok(c.ret_ty.unwrap_or_else(|| Type::any(span).owned()));
                            }
                        }

                        unimplemented!(
                            "multiple methods with same name and same number of arguments"
                        )
                    }
                }
            }};
        }

        match *callee {
            Expr::Ident(ref i) if i.sym == js_word!("require") => {
                if let Some(dep) = self.resolved_imports.get(
                    &args
                        .iter()
                        .cloned()
                        .map(|arg| match arg {
                            ExprOrSpread { spread: None, expr } => match *expr {
                                Expr::Lit(Lit::Str(Str { value, .. })) => value.clone(),
                                _ => unimplemented!("dynamic import: require()"),
                            },
                            _ => unimplemented!("error reporting: spread element in require()"),
                        })
                        .next()
                        .unwrap(),
                ) {
                    let dep = dep.clone();
                    unimplemented!("dep: {:#?}", dep);
                }

                // if let Some(Type::Enum(ref e)) = self.scope.find_type(&i.sym) {
                //     return Ok(TsType::TsTypeRef(TsTypeRef {
                //         span,
                //         type_name: TsEntityName::Ident(i.clone()),
                //         type_params: None,
                //     })
                //     .into());
                // }
                Err(Error::UndefinedSymbol { span: i.span() })
            }

            Expr::Member(MemberExpr {
                obj: ExprOrSuper::Expr(ref obj),
                ref prop,
                computed,
                ..
            }) => {
                // member expression
                let obj_type = self.type_of(obj)?.generalize_lit();

                match *obj_type.normalize() {
                    Type::Function(ref f) if kind == ExtractKind::Call => {
                        //
                        return Ok(*f.ret_ty.clone());
                    }

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {
                        return Ok(Type::any(span).into_cow());
                    }

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) => {
                        return Ok(
                            builtin_types::get_type(self.libs, span, &js_word!("Number"))
                                .map(Type::owned)
                                .expect("Builtin type named 'Numnber' should exist"),
                        );
                    }

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    }) => {
                        return Ok(
                            builtin_types::get_type(self.libs, span, &js_word!("String"))
                                .map(Type::owned)
                                .expect("Builtin type named 'String' should exist"),
                        );
                    }

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                        ..
                    }) => {
                        if let Ok(ty) =
                            builtin_types::get_type(self.libs, span, &js_word!("Symbol"))
                        {
                            return Ok(ty.owned());
                        }
                    }

                    Type::Interface(ref i) => {
                        search_members_for_prop!(&i.body, prop);
                    }

                    Type::TypeLit(ref t) => {
                        search_members_for_prop!(&t.members, prop);
                    }

                    Type::Class(Class { ref body, .. }) => {
                        for member in body.iter() {
                            match *member {
                                ClassMember::Method(ClassMethod {
                                    ref key,
                                    ref function,
                                    ..
                                }) => {
                                    if prop_name_to_expr(key).eq_ignore_span(&*prop) {
                                        if let Some(ref ret_ty) = function.return_type {
                                            return Ok(Type::from(ret_ty.clone()).owned());
                                        } else {
                                            return Ok(Type::any(span).owned());
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }

                    _ => {}
                }

                if computed {
                    unimplemented!("typeeof(CallExpr): {:?}[{:?}]()", callee, prop)
                } else {
                    println!(
                        "extract_call_or_new_expr: \nobj_type: {:?}\ntype_of(callee): {:?}",
                        obj_type,
                        self.type_of(callee)?
                    );

                    Err(if kind == ExtractKind::Call {
                        Error::NoCallSignature {
                            span,
                            callee: self.type_of(callee)?.to_static(),
                        }
                    } else {
                        Error::NoNewSignature {
                            span,
                            callee: self.type_of(callee)?.to_static(),
                        }
                    })
                }
            }
            _ => {
                let ty = self.type_of(callee)?;
                let ty = self.expand_type(span, ty)?;

                Ok(self.extract(span, &ty, kind, args, type_args)?.into_cow())
            }
        }
    }

    fn extract<'a>(
        &'a self,
        span: Span,
        ty: &Type<'a>,
        kind: ExtractKind,
        args: &[ExprOrSpread],
        type_args: Option<&TsTypeParamInstantiation>,
    ) -> Result<Type, Error> {
        if cfg!(debug_assertions) {
            match *ty.normalize() {
                Type::Simple(ref s) => match **s {
                    TsType::TsTypeRef(ref s) => unreachable!("TypeRef: {:#?}", s),
                    _ => {}
                },
                _ => {}
            }
        }

        macro_rules! ret_err {
            () => {{
                println!("ret_err!(): {:?}", ty);

                match kind {
                    ExtractKind::Call => {
                        return Err(Error::NoCallSignature {
                            span,
                            callee: ty.to_static(),
                        })
                    }
                    ExtractKind::New => {
                        return Err(Error::NoNewSignature {
                            span,
                            callee: ty.to_static(),
                        })
                    }
                }
            }};
        }

        /// Search for members and returns if there's a match
        macro_rules! search_members {
            ($members:expr) => {{
                for member in &$members {
                    match *member {
                        TypeElement::Call(CallSignature {
                            ref params,
                            ref type_params,
                            ref ret_ty,
                            ..
                        }) if kind == ExtractKind::Call => {
                            //
                            match self.try_instantiate_simple(
                                span,
                                ty.span(),
                                &ret_ty.as_ref().unwrap_or(&Type::any(span).owned()),
                                params,
                                type_params.as_ref(),
                                args,
                                type_args,
                            ) {
                                Ok(v) => return Ok(v),
                                Err(..) => {}
                            };
                        }

                        TypeElement::Constructor(ConstructorSignature {
                            ref params,
                            ref type_params,
                            ref ret_ty,
                            ..
                        }) if kind == ExtractKind::New => {
                            match self.try_instantiate_simple(
                                span,
                                ty.span(),
                                &ret_ty.as_ref().unwrap_or(&Type::any(span).owned()),
                                params,
                                type_params.as_ref(),
                                args,
                                type_args,
                            ) {
                                Ok(v) => return Ok(v),
                                Err(..) => {
                                    // TODO: Handle error
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }};
        }

        match *ty.normalize() {
            Type::Static(..) => unreachable!("normalize should handle Type::Static"),

            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) => return Ok(Type::any(span)),

            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsUnknownKeyword,
                ..
            }) => return Err(Error::Unknown { span }),

            Type::Function(ref f) if kind == ExtractKind::Call => {
                self.try_instantiate(span, ty.span(), &*f, args, type_args)
            }

            Type::Constructor(ty::Constructor {
                ref params,
                ref type_params,
                ref ret_ty,
                ..
            }) if kind == ExtractKind::New => self.try_instantiate_simple(
                span,
                ty.span(),
                &ret_ty,
                params,
                type_params.as_ref(),
                args,
                type_args,
            ),

            Type::Union(ref u) => {
                let mut errors = vec![];
                for ty in &u.types {
                    match self.extract(span, &ty, kind, args, type_args) {
                        Ok(ty) => return Ok(ty),
                        Err(err) => errors.push(err),
                    }
                }

                Err(Error::UnionError { span, errors })
            }

            Type::Interface(ref i) => {
                // Search for methods
                search_members!(i.body);

                ret_err!()
            }

            Type::TypeLit(ref l) => {
                search_members!(l.members);

                ret_err!()
            }

            Type::Class(..) if kind == ExtractKind::New => {
                // The class does not have a constructor.
                // We should check parent class' constructor.

                let mut iter_ty = Some(Cow::Borrowed(ty));
                while let Some(ref t) = iter_ty.as_ref().map(|ty| ty.normalize()) {
                    match t {
                        Type::Class(ref c) => {
                            for member in c.body.iter() {
                                match member {
                                    ClassMember::Constructor(..) => unimplemented!("new Class()"),
                                    _ => {}
                                }
                            }

                            iter_ty = match c.super_class {
                                Some(ref sc) => Some(self.type_of(&sc)?),
                                None => None,
                            };
                        }

                        _ => unimplemented!(
                            "error reporting: Calss extends something other than class"
                        ),
                    }
                }

                return Ok(ty.clone());
            }

            Type::Simple(ref sty) => match **sty {
                TsType::TsTypeQuery(TsTypeQuery {
                    expr_name: TsEntityName::Ident(Ident { ref sym, .. }),
                    ..
                }) => {
                    if self.scope.find_declaring_fn(sym) {
                        return Ok(Type::any(span));
                    }

                    ret_err!();
                }
                _ => ret_err!(),
            },

            _ => ret_err!(),
        }
    }

    fn try_instantiate_simple<'a>(
        &'a self,
        span: Span,
        callee_span: Span,
        ret_type: &Type<'a>,
        param_decls: &[TsFnParam],
        decl: Option<&TypeParamDecl>,
        args: &[ExprOrSpread],
        _: Option<&TsTypeParamInstantiation>,
    ) -> Result<Type<'a>, Error> {
        {
            // let type_params_len = ty_params_decl.map(|decl|
            // decl.params.len()).unwrap_or(0); let type_args_len = i.map(|v|
            // v.params.len()).unwrap_or(0);

            // // TODO: Handle multiple definitions
            // let min = ty_params_decl
            //     .map(|decl| decl.params.iter().filter(|p|
            // p.default.is_none()).count())
            //     .unwrap_or(type_params_len);

            // let expected = min..=type_params_len;
            // if !expected.contains(&type_args_len) {
            //     return Err(Error::WrongTypeParams {
            //         span,
            //         callee: callee_span,
            //         expected,
            //         actual: type_args_len,
            //     });
            // }
        }

        {
            // TODO: Handle default parameters
            // TODO: Handle multiple definitions

            let min = param_decls
                .iter()
                .filter(|p| match p {
                    TsFnParam::Ident(Ident { optional: true, .. }) => false,
                    _ => true,
                })
                .count();

            let expected = min..=param_decls.len();
            if !expected.contains(&args.len()) {
                return Err(Error::WrongParams {
                    span,
                    callee: callee_span,
                    expected,
                    actual: args.len(),
                });
            }
        }

        if let Some(..) = decl {
            unimplemented!(
                "try_instantiate should be used instead of try_instantiate_simple as type \
                 parameter is deefined on the function"
            )
        } else {
            Ok(ret_type.clone())
        }
    }

    fn try_instantiate<'a>(
        &'a self,
        span: Span,
        callee_span: Span,
        fn_type: &ty::Function<'a>,
        args: &[ExprOrSpread],
        i: Option<&TsTypeParamInstantiation>,
    ) -> Result<Type<'a>, Error> {
        let param_decls = &fn_type.params;
        let decl = &fn_type.type_params;
        let type_params = if let Some(ref type_params) = fn_type.type_params {
            type_params
        } else {
            // TODO: Report an error if i is not None
            return Ok((*fn_type.ret_ty).clone().into_owned());
        };

        {
            // TODO: Handle default parameters
            // TODO: Handle multiple definitions

            let min = param_decls
                .iter()
                .filter(|p| match p {
                    TsFnParam::Ident(Ident { optional: true, .. }) => false,
                    _ => true,
                })
                .count();

            let expected = min..=param_decls.len();
            if !expected.contains(&args.len()) {
                return Err(Error::WrongParams {
                    span,
                    callee: callee_span,
                    expected,
                    actual: args.len(),
                });
            }
        }

        let v;

        let i = match i {
            Some(i) => i,
            None => {
                v = self.infer_arg_types(args, &type_params, &fn_type.params)?;
                &v
            }
        };

        if let Some(ref decl) = decl {
            // To handle
            //
            // function foo<T extends "foo">(f: (x: T) => T) {
            //     return f;
            // }
            //
            // we should expand the whole function, because return type contains type
            // parameter declared on the function.
            let expanded_fn_type = self
                .expand_type_params(i, decl, Cow::Owned(Type::Function(fn_type.clone())))?
                .into_owned();
            let expanded_fn_type = match expanded_fn_type {
                Type::Function(f) => f,
                _ => unreachable!(),
            };
            Ok((*expanded_fn_type.ret_ty).clone().into_owned())
        } else {
            Ok((*fn_type.ret_ty).clone().into_owned())
        }
    }

    /// Expands
    ///
    ///   - Type alias
    pub(super) fn expand_type<'t>(
        &'t self,
        span: Span,
        ty: TypeRef<'t>,
    ) -> Result<TypeRef<'t>, Error> {
        // println!("({}) expand({:?})", self.scope.depth(), ty);

        macro_rules! verify {
            ($ty:expr) => {{
                if cfg!(debug_assertions) {
                    match $ty.normalize() {
                        Type::Simple(ref s) => match **s {
                            TsType::TsTypeRef(..) => unreachable!(),
                            _ => {}
                        },
                        _ => {}
                    }
                }
            }};
        }

        match *ty {
            Type::Static(s) => return self.expand_type(span, s.ty.static_cast()),
            Type::Simple(ref s_ty) => {
                match **s_ty {
                    TsType::TsTypeRef(TsTypeRef {
                        ref type_name,
                        ref type_params,
                        ..
                    }) => {
                        match *type_name {
                            TsEntityName::Ident(ref i) => {
                                // Check for builtin types
                                if let Ok(ty) = builtin_types::get_type(self.libs, span, &i.sym) {
                                    verify!(ty);
                                    return Ok(ty.owned());
                                }

                                // Handle enum
                                if let Some(ref ty) = self.find_type(&i.sym) {
                                    match ty.normalize() {
                                        Type::Enum(..) => {
                                            if let Some(..) = *type_params {
                                                return Err(Error::NotGeneric { span });
                                            }
                                            verify!(ty);
                                            return Ok(ty.static_cast());
                                        }

                                        Type::Param(..) => {
                                            if let Some(..) = *type_params {
                                                return Err(Error::NotGeneric { span });
                                            }

                                            verify!(ty);
                                            return Ok(ty.static_cast());
                                        }

                                        Type::Interface(..) | Type::Class(..) => {
                                            // TODO: Handle type parameters
                                            verify!(ty);
                                            return Ok(ty.static_cast());
                                        }

                                        Type::Alias(Alias {
                                            type_params: None,
                                            ref ty,
                                            ..
                                        }) => {
                                            verify!(ty);
                                            return Ok(ty.static_cast());
                                        }

                                        // Expand type parameters.
                                        Type::Alias(Alias {
                                            type_params: Some(ref tps),
                                            ref ty,
                                            ..
                                        }) => {
                                            let ty = if let Some(i) = type_params {
                                                self.expand_type_params(
                                                    i,
                                                    tps,
                                                    Cow::Borrowed(&**ty),
                                                )?
                                            } else {
                                                *ty.clone()
                                            };
                                            let ty = self.expand_type(span, ty.static_cast())?;

                                            verify!(ty);
                                            return Ok(ty.to_static().owned());
                                        }

                                        _ => unimplemented!(
                                            "Handling result of find_type() -> {:#?}",
                                            ty
                                        ),
                                    }
                                }
                            }

                            // Handle enum variant type.
                            //
                            //  let a: StringEnum.Foo = x;
                            TsEntityName::TsQualifiedName(box TsQualifiedName {
                                left: TsEntityName::Ident(ref left),
                                ref right,
                            }) => {
                                if let Some(ref ty) = self.scope.find_type(&left.sym) {
                                    match *ty {
                                        Type::Enum(..) => {
                                            return Ok(EnumVariant {
                                                span,
                                                enum_name: left.sym.clone(),
                                                name: right.sym.clone(),
                                            }
                                            .into_cow())
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }

                        unimplemented!(
                            "expand_type(): error reporting for type not found: {:#?}",
                            ty
                        );
                    }

                    TsType::TsTypeQuery(TsTypeQuery { ref expr_name, .. }) => match *expr_name {
                        TsEntityName::Ident(ref i) => {
                            let ty = self.type_of(&Expr::Ident(i.clone()))?;

                            return Ok(ty);
                        }
                        _ => unimplemented!("expand(TsTypeQuery): typeof member.expr"),
                    },

                    _ => {
                        return Ok(Cow::Owned((*s_ty).clone().into()));
                    }
                }
            }

            _ => {}
        }

        let ty = match ty.into_owned() {
            Type::Union(Union { span, types }) => {
                let v = types
                    .into_iter()
                    .map(|ty| Ok(self.expand_type(span, ty)?.into_owned()))
                    .collect::<Result<Vec<_>, _>>()?;
                return Ok(Type::union(v).into_cow());
            }
            Type::Intersection(Intersection { span, types }) => {
                return Ok(Intersection {
                    span,
                    types: types
                        .into_iter()
                        .map(|ty| Ok(self.expand_type(span, ty)?))
                        .collect::<Result<_, _>>()?,
                }
                .into_cow())
            }

            Type::Array(Array {
                span,
                box elem_type,
            }) => {
                let elem_type = box self.expand_type(elem_type.span(), elem_type)?;
                return Ok(Array { span, elem_type }.into_cow());
            }

            ty => ty,
        };

        Ok(ty.into_cow())
    }
}

fn prop_key_to_expr(p: &Prop) -> Box<Expr> {
    match *p {
        Prop::Shorthand(ref i) => box Expr::Ident(i.clone()),
        Prop::Assign(AssignProp { ref key, .. }) => box Expr::Ident(key.clone()),
        Prop::Getter(GetterProp { ref key, .. })
        | Prop::KeyValue(KeyValueProp { ref key, .. })
        | Prop::Method(MethodProp { ref key, .. })
        | Prop::Setter(SetterProp { ref key, .. }) => prop_name_to_expr(key),
    }
}

fn prop_name_to_expr(key: &PropName) -> Box<Expr> {
    match *key {
        PropName::Computed(ref expr) => expr.clone(),
        PropName::Ident(ref ident) => box Expr::Ident(ident.clone()),
        PropName::Str(ref s) => box Expr::Lit(Lit::Str(Str { ..s.clone() })),
        PropName::Num(ref s) => box Expr::Lit(Lit::Num(Number { ..s.clone() })),
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
                })
            }
            TsLit::Number(v) => {
                return Type::Lit(TsLitType {
                    lit: TsLit::Bool(Bool {
                        value: v.value != 0.0,
                        span: v.span,
                    }),
                    span,
                })
            }
            TsLit::Str(ref v) => {
                return Type::Lit(TsLitType {
                    lit: TsLit::Bool(Bool {
                        value: v.value != js_word!(""),
                        span: v.span,
                    }),
                    span,
                })
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExtractKind {
    Call,
    New,
}

impl Visit<Stmt> for Analyzer<'_, '_> {
    fn visit(&mut self, stmt: &Stmt) {
        stmt.visit_children(self);

        match *stmt {
            // Validate expression statements
            Stmt::Expr(ref expr) => match self.type_of(&expr) {
                Ok(..) => {}
                Err(err) => {
                    self.info.errors.push(err);
                    return;
                }
            },

            _ => {}
        }
    }
}

impl Visit<SeqExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, expr: &SeqExpr) {
        let first_span = expr.exprs[0].span();

        for expr in expr.exprs[..expr.exprs.len() - 1].iter() {
            let span = expr.span();
            expr.visit_with(self);

            match **expr {
                Expr::Ident(..) | Expr::Lit(..) => {
                    self.info.errors.push(Error::UselessSeqExpr {
                        span: span.with_lo(first_span.lo()),
                    });
                }

                _ => {}
            }
        }
    }
}

impl Visit<ClassProp> for Analyzer<'_, '_> {
    fn visit(&mut self, prop: &ClassProp) {
        match *prop.key {
            Expr::Ident(Ident { ref sym, .. }) => self.scope.declaring_prop = Some(sym.clone()),
            _ => {}
        }

        prop.visit_children(self);

        match prop.value {
            Some(ref value) => match self.type_of(value) {
                Ok(..) => {}
                Err(err) => {
                    self.info.errors.push(err);
                }
            },
            _ => {}
        }

        self.scope.declaring_prop = None;
    }
}

impl Visit<TsTypeAssertion> for Analyzer<'_, '_> {
    fn visit(&mut self, expr: &TsTypeAssertion) {
        // TODO: Check type
        expr.visit_children(self);
    }
}

impl Visit<TsAsExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, expr: &TsAsExpr) {
        // TODO: Check type
        expr.visit_children(self);
    }
}
