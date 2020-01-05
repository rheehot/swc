//! Handles new expressions and call expressions.
use super::super::Analyzer;
use crate::{
    analyzer::props::prop_name_to_expr,
    builtin_types,
    errors::Error,
    ty,
    ty::{
        CallSignature, ClassInstance, ConstructorSignature, Method, MethodSignature, Static, Type,
        TypeElement,
    },
    util::EqIgnoreSpan,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use swc_atoms::js_word;
use swc_common::{Span, Spanned};
use swc_ecma_ast::*;
use swc_ts_checker_macros::validator;

prevent!(CallExpr);
prevent!(NewExpr);

impl Validate<CallExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &CallExpr) -> ValidationResult {
        let CallExpr {
            span,
            ref callee,
            ref args,
            ref type_args,
        } = *e;

        let callee = match callee {
            ExprOrSuper::Super(..) => return Ok(Type::any(span)),
            ExprOrSuper::Expr(callee) => callee,
        };

        // TODO: validate children

        // Check arguments
        for arg in &e.args {
            let res: Result<(), Error> = try {
                self.validate(&arg.expr)?;
            };

            if let Err(err) = res {
                self.info.errors.push(err);
            }
        }

        // Check callee
        let callee_ty = self.validate(&callee)?;
        match *callee_ty.normalize() {
            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) if e.type_args.is_some() => self.info.errors.push(Error::TS2347 { span: e.span }),
            _ => {}
        }

        self.extract_call_new_expr_member(callee, ExtractKind::Call, args, type_args.as_ref())
    }
}

impl Validate<NewExpr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &NewExpr) -> ValidationResult {
        let NewExpr {
            span,
            ref callee,
            ref args,
            ref type_args,
        } = *e;

        // TODO: e.visit_children

        self.check_callee(e.span, &e.callee, e.type_args.as_ref());

        // Check arguments
        if let Some(ref args) = e.args {
            for arg in args {
                let res: Result<(), Error> = try {
                    self.validate(&arg.expr)?;
                };

                if let Err(err) = res {
                    self.info.errors.push(err);
                }
            }
        }

        self.extract_call_new_expr_member(
            callee,
            ExtractKind::New,
            args.as_ref().map(|v| &**v).unwrap_or_else(|| &[]),
            type_args.as_ref(),
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExtractKind {
    New,
    Call,
}

impl Analyzer<'_, '_> {
    #[validator]
    fn check_callee(
        &mut self,
        span: Span,
        callee: &Expr,
        type_args: Option<&TsTypeParamInstantiation>,
    ) {
        let callee_ty = self.validate(callee)?;
        match *callee_ty.normalize() {
            Type::Keyword(TsKeywordType {
                kind: TsKeywordTypeKind::TsAnyKeyword,
                ..
            }) if type_args.is_some() => Err(Error::TS2347 { span })?,
            _ => {}
        }
    }

    /// Calculates the return type of a new /call expression.
    ///
    /// Called only from [type_of_expr]
    fn extract_call_new_expr_member(
        &mut self,
        callee: &Expr,
        kind: ExtractKind,
        args: &[ExprOrSpread],
        type_args: Option<&TsTypeParamInstantiation>,
    ) -> ValidationResult {
        let span = callee.span();

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
                let is_key_eq_prop = |e: &Expr| {
                    let v = match *e {
                        Expr::Ident(ref i) => &i.sym,
                        Expr::Lit(Lit::Str(ref s)) => &s.value,
                        _ => return false,
                    };

                    let p = match **prop {
                        Expr::Ident(ref i) => &i.sym,
                        Expr::Lit(Lit::Str(ref s)) if computed => &s.value,
                        _ => return false,
                    };

                    v == p
                };

                macro_rules! search_members_for_prop {
                    ($members:expr) => {{
                        // Candidates of the method call.
                        //
                        // 4 is just an unscientific guess
                        // TODO: Use smallvec
                        let mut candidates = Vec::with_capacity(4);

                        macro_rules! check {
                            ($m:expr) => {{
                                match $m {
                                    TypeElement::Method(ref m) if kind == ExtractKind::Call => {
                                        // We are interested only on methods named `prop`
                                        if is_key_eq_prop(&m.key) {
                                            candidates.push(m.clone());
                                        }
                                    }

                                    _ => {}
                                }
                            }};
                        }

                        for m in $members {
                            check!(m);
                        }

                        {
                            // Handle methods from `interface Object`
                            let i = builtin_types::get_type(self.libs, span, &js_word!("Object"))
                                .expect("`interface Object` is must");
                            let methods = match i {
                                Type::Static(Static {
                                    ty: Type::Interface(i),
                                    ..
                                }) => &*i.body,

                                _ => &[],
                            };

                            // TODO: Remove clone
                            for m in methods.into_iter().map(|v| v.clone()) {
                                check!(m);
                            }
                        }

                        match candidates.len() {
                            0 => {
                                unimplemented!("no method with same name\nMembers: {:?}", $members)
                            }
                            1 => {
                                // TODO:
                                return self.check_method_call(
                                    span,
                                    &candidates.into_iter().next().unwrap(),
                                    args,
                                );
                            }
                            _ => {
                                //
                                for c in candidates {
                                    if c.params.len() == args.len() {
                                        return self.check_method_call(span, &c, args);
                                    }
                                }

                                unimplemented!(
                                    "multiple methods with same name and same number of arguments"
                                )
                            }
                        }
                    }};
                }

                {
                    // Handle toString()
                    macro_rules! handle {
                        () => {{
                            return Ok(Type::from(TsKeywordType {
                                span,
                                kind: TsKeywordTypeKind::TsStringKeyword,
                            }));
                        }};
                    }
                    match **prop {
                        Expr::Ident(Ident {
                            sym: js_word!("toString"),
                            ..
                        }) if !computed => handle!(),
                        Expr::Lit(Lit::Str(Str {
                            value: js_word!("toString"),
                            ..
                        })) => handle!(),

                        _ => {}
                    }
                }

                // Handle member expression
                let obj_type = self.validate(obj)?.generalize_lit();

                let obj_type = match *obj_type.normalize() {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) => builtin_types::get_type(self.libs, span, &js_word!("Number"))
                        .expect("Builtin type named 'Number' should exist"),
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsStringKeyword,
                        ..
                    }) => builtin_types::get_type(self.libs, span, &js_word!("String"))
                        .expect("Builtin type named 'String' should exist"),
                    _ => obj_type,
                };

                match *obj_type.normalize() {
                    Type::Function(ref f) if kind == ExtractKind::Call => {
                        //
                        return Ok(*f.ret_ty.clone());
                    }

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {
                        return Ok(Type::any(span));
                    }

                    Type::Interface(ref i) => {
                        // TODO: Check parent interface
                        search_members_for_prop!(i.body.iter());
                    }

                    Type::TypeLit(ref t) => {
                        search_members_for_prop!(t.members.iter());
                    }

                    Type::Class(ty::Class { ref body, .. }) => {
                        for member in body.iter() {
                            match *member {
                                ty::ClassMember::Method(Method {
                                    ref key,
                                    ref ret_ty,
                                    ..
                                }) => {
                                    if prop_name_to_expr(key).eq_ignore_span(&*prop) {
                                        return Ok(*ret_ty.clone());
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsSymbolKeyword,
                        ..
                    }) => {
                        if let Ok(ty) =
                            builtin_types::get_type(self.libs, span, &js_word!("Symbol"))
                        {
                            return Ok(ty);
                        }
                    }

                    _ => {}
                }

                if computed {
                    unimplemented!("typeof(CallExpr): {:?}[{:?}]()", callee, prop)
                } else {
                    println!("extract_call_or_new_expr: \nobj_type: {:?}", obj_type,);

                    Err(if kind == ExtractKind::Call {
                        Error::NoCallSignature {
                            span,
                            callee: self.validate(callee)?,
                        }
                    } else {
                        Error::NoNewSignature {
                            span,
                            callee: self.validate(callee)?,
                        }
                    })
                }
            }
            _ => {
                let ty = callee.validate_with(self)?;

                Ok(self.extract(span, ty, kind, args, type_args)?)
            }
        }
    }

    fn extract(
        &mut self,
        span: Span,
        ty: Type,
        kind: ExtractKind,
        args: &[ExprOrSpread],
        type_args: Option<&TsTypeParamInstantiation>,
    ) -> ValidationResult {
        if cfg!(debug_assertions) {
            match *ty.normalize() {
                Type::Ref(ref s) => unreachable!("Type::Ref: {:#?}", s),
                _ => {}
            }
        }

        macro_rules! ret_err {
            () => {{
                match kind {
                    ExtractKind::Call => return Err(Error::NoCallSignature { span, callee: ty }),
                    ExtractKind::New => return Err(Error::NoNewSignature { span, callee: ty }),
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
                                &ret_ty.as_ref().unwrap_or(&Type::any(span)),
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
                                &ret_ty.as_ref().unwrap_or(&Type::any(span)),
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

            // Type::Constructor(ty::Constructor {
            //     ref params,
            //     ref type_params,
            //     ref ret_ty,
            //     ..
            // }) if kind == ExtractKind::New => self.try_instantiate_simple(
            //     span,
            //     ty.span(),
            //     &ret_ty,
            //     params,
            //     type_params.as_ref(),
            //     args,
            //     type_args,
            // ),
            Type::Union(ref u) => {
                let mut errors = vec![];
                for ty in &u.types {
                    // TODO: Remove clone
                    match self.extract(span, ty.clone(), kind, args, type_args) {
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

            Type::Class(ref cls) if kind == ExtractKind::New => {
                // TODO: Remove clone
                return Ok(ClassInstance {
                    span,
                    cls: cls.clone(),
                    type_args: try_opt!(type_args.validate_with(self)),
                }
                .into());
            }

            Type::Query(TsTypeQuery {
                expr_name: TsTypeQueryExpr::TsEntityName(TsEntityName::Ident(Ident { ref sym, .. })),
                ..
            }) => {
                //if self.scope.find_declaring_fn(sym) {
                //    return Ok(Type::any(span));
                //}

                ret_err!();
            }

            _ => ret_err!(),
        }
    }

    fn check_method_call(
        &mut self,
        span: Span,
        c: &MethodSignature,
        args: &[ExprOrSpread],
    ) -> ValidationResult {
        // Validate arguments
        for (i, p) in c.params.into_iter().enumerate() {
            // TODO: Handle spread
            // TODO: Validate optional parameters
            if args.len() > i {
                let args_ty = args[i].expr.validate_with(self)?;
                self.assign(&p.ty, &args_ty, args[i].span())?;
            }
        }

        return Ok(c.ret_ty.unwrap_or_else(|| Type::any(span).owned()));
    }
}
