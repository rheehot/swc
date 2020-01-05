use super::{
    expr::TypeOfMode,
    scope::ScopeKind,
    util::{is_prop_name_eq, VarVisitor},
    Analyzer,
};
use crate::{
    errors::Error,
    swc_common::VisitWith,
    ty,
    ty::Type,
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use std::mem::replace;
use swc_atoms::js_word;
use swc_common::{util::move_map::MoveMap, Fold, Span, Spanned, DUMMY_SP};
use swc_common::{util::move_map::MoveMap, Span, Spanned, Visit, DUMMY_SP};
use swc_common::{util::move_map::MoveMap, Fold, Span, Spanned, Visit, DUMMY_SP};
use swc_atoms::{js_word, JsWord};
use swc_common::{Span, Spanned, Visit, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ts_checker_macros::validator;

prevent!(ClassProp);
prevent!(Constructor);
prevent!(ClassMethod);
prevent!(TsFnParam);
prevent!(TsIndexSignature);

impl Validate<ClassProp> for Analyzer<'_, '_> {
    type Output = ValidationResult<ty::ClassProperty>;

    fn validate(&mut self, p: &ClassProp) -> Self::Output {
        p.validate_children(self);

        // Verify key if key is computed
        if p.computed {
            self.validate_computed_prop_key(p.span, &p.key);
        }

        //if let Some(ref ty) = p.type_ann {
        //    let span = ty.span();
        //
        //    let ty: Type = ty.type_ann.clone().into();
        //    self.expand_type(span, ty).store(&mut errors);
        //}

        let value: Option<Type> = try_opt!(self.validate(&p.value));

        Ok(ty::ClassProperty {
            span: p.span,
            key: p.key.clone(),
            value,
            is_static: p.is_static,
            computed: p.computed,
            accessibility: p.accessibility,
            is_abstract: p.is_abstract,
            is_optional: p.is_optional,
            readonly: p.readonly,
            definite: p.definite,
        })
    }
}

impl Validate<Constructor> for Analyzer<'_, '_> {
    type Output = ValidationResult<ty::Constructor>;

    fn validate(&mut self, c: &Constructor) -> Self::Output {
        let c_span = c.span();

        self.with_child(ScopeKind::Fn, Default::default(), |child| {
            let Constructor { params, .. } = c;

            {
                // Validate params
                // TODO: Move this to parser
                let mut has_optional = false;
                for p in params {
                    if has_optional {
                        child.info.errors.push(Error::TS1016 { span: p.span() });
                    }

                    match *p {
                        PatOrTsParamProp::Pat(Pat::Ident(Ident { optional, .. })) => {
                            if optional {
                                has_optional = true;
                            }
                        }
                        _ => {}
                    }
                }
            }

            for param in params {
                let mut names = vec![];

                let mut visitor = VarVisitor { names: &mut names };

                param.visit_with(&mut visitor);

                child.scope.declaring.extend(names.clone());

                match param {
                    PatOrTsParamProp::Pat(ref pat) => {
                        match child.declare_vars(VarDeclKind::Let, pat) {
                            Ok(()) => {}
                            Err(err) => {
                                child.info.errors.push(err);
                            }
                        }
                    }
                    PatOrTsParamProp::TsParamProp(ref param) => match param.param {
                        TsParamPropParam::Ident(ref i)
                        | TsParamPropParam::Assign(AssignPat {
                            left: box Pat::Ident(ref i),
                            ..
                        }) => {
                            let ty = try_opt!(self.validate(&i.type_ann));
                            //let ty = match ty {
                            //    Some(ty) => match child.expand_type(i.span, ty) {
                            //        Ok(ty) => Some(ty),
                            //        Err(err) => {
                            //            child.info.errors.push(err);
                            //            Some(Type::any(i.span))
                            //        }
                            //    },
                            //    None => None,
                            //};

                            match child.scope.declare_var(
                                i.span,
                                VarDeclKind::Let,
                                i.sym.clone(),
                                ty,
                                true,
                                false,
                            ) {
                                Ok(()) => {}
                                Err(err) => {
                                    child.info.errors.push(err);
                                }
                            }
                        }
                        _ => unreachable!(),
                    },
                }

                child.scope.remove_declaring(names);
            }

            child.inferred_return_types.get_mut().insert(c_span, vec![]);
            let c = Constructor { params, ..c }.fold_children(child);
            let c = Constructor { params, ..c }.visit_children(child);

            Ok(())
            Ok(ty::Constructor {})
            Ok(ty::Constructor {
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
                    .map(|param| self.validate(&param))
                    .collect::<Result<_, _>>()?,
            })
        })
    }
}

impl Validate<TsFnParam> for Analyzer<'_, '_> {
    type Output = ValidationResult<ty::FnParam>;

    fn validate(&mut self, p: &TsFnParam) -> Self::Output {
        unimplemented!("validate(TsFnParam)")
    }
}

impl Validate<Pat> for Analyzer<'_, '_> {
    type Output = ValidationResult<ty::FnParam>;

    fn validate(&mut self, p: &Pat) -> Self::Output {
        unimplemented!("validate(Pat)")
    }
}

impl Validate<ClassMethod> for Analyzer<'_, '_> {
    type Output = ValidationResult<ty::Method>;

    fn validate(&mut self, c: &ClassMethod) -> Self::Output {
        let c_span = c.span();
        let key_span = c.key.span();

        self.with_child(ScopeKind::Fn, Default::default(), |child| {
            {
                // Validate params
                // TODO: Move this to parser
                let mut has_optional = false;
                for p in &c.function.params {
                    if has_optional {
                        child.info.errors.push(Error::TS1016 { span: p.span() });
                    }

                    match *p {
                        Pat::Ident(Ident { optional, .. }) => {
                            if optional {
                                has_optional = true;
                            }
                        }
                        _ => {}
                    }
                }
            }

            c.function.params.iter().for_each(|pat| {
                let mut names = vec![];

                let mut visitor = VarVisitor { names: &mut names };

                pat.visit_with(&mut visitor);

                child.scope.declaring.extend(names.clone());

                match child.declare_vars(VarDeclKind::Let, pat) {
                    Ok(()) => {}
                    Err(err) => {
                        child.info.errors.push(err);
                    }
                }

                child.scope.remove_declaring(names);
            });

            child.inferred_return_types.get_mut().insert(c.span, vec![]);
            c.key = c.key.fold_with(child);
            c.function = c.function.fold_children(child);
            c.key.visit_with(child);
            c.function.visit_children(child);
            c.key = c.key.visit_with(child);
            c.function = c.function.visit_children(child);

            debug_assert_eq!(child.allow_ref_declaring, false);
            child.allow_ref_declaring = old;

            (
                child
                    .inferred_return_types
                    .get_mut()
                    .remove_entry(&c_span)
                    .unwrap_or_default(),
                c,
            )
        });

        if c.kind == MethodKind::Getter && c.function.body.is_some() {
            let ret_ty = self.visit_stmts_for_return(&c.function.body.as_ref().unwrap().stmts)?;

            // getter property must have return statements.
            if let None = ret_ty {
                self.info
                    .errors
                    .push(Error::GetterPropWithoutReturn { span: key_span });
            }
        }

        unimplemented!("validate_class_method")
    }
}

impl Validate<ClassMember> for Analyzer<'_, '_> {
    type Output = ValidationResult<Option<ty::ClassMember>>;

    fn validate(&mut self, m: &ClassMember) -> Self::Output {
        Ok(match m {
            swc_ecma_ast::ClassMember::PrivateMethod(_)
            | swc_ecma_ast::ClassMember::PrivateProp(_) => None,

            swc_ecma_ast::ClassMember::Constructor(v) => {
                Some(ty::ClassMember::Constructor(v.validate_with(self)?))
            }
            swc_ecma_ast::ClassMember::Method(v) => {
                Some(ty::ClassMember::Method(v.validate_with(self)?))
            }
            swc_ecma_ast::ClassMember::ClassProp(v) => {
                Some(ty::ClassMember::Property(v.validate_with(self)?))
            }
            swc_ecma_ast::ClassMember::TsIndexSignature(v) => {
                Some(ty::ClassMember::IndexSignature(v.validate_with(self)?))
            }
        })
    }
}

impl Analyzer<'_, '_> {
    /// In almost case, this method returns `Ok`.
    pub(super) fn validate_type_of_class(
        &mut self,
        name: Option<JsWord>,
        c: &swc_ecma_ast::Class,
    ) -> ValidationResult<ty::Class> {
        for m in c.body.iter() {
            match *m {
                swc_ecma_ast::ClassMember::ClassProp(ref prop) => match prop.type_ann {
                    Some(ref ty) => {
                        let ty = self.validate(ty)?;
                        if ty.is_any() || ty.is_unknown() {
                        } else {
                            if prop.value.is_none() {
                                // TODO: Uncomment this after implementing a
                                // constructor checker.
                                // self.info
                                //     .errors
                                //     .push(Error::ClassPropertyInitRequired {
                                // span })
                            }
                        }
                    }
                    None => {}
                },
                _ => {}
            }
        }

        self.type_of_class(name, c)
    }

    pub(super) fn type_of_class(
        &mut self,
        name: Option<JsWord>,
        c: &swc_ecma_ast::Class,
    ) -> ValidationResult<ty::Class> {
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
        //                     type_ann: box ty,
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

        let super_class = match c.super_class {
            Some(ref expr) => Some(box self.validate(&expr)?),
            None => None,
        };

        // TODO: Check for implements

        Ok(ty::Class {
            span: c.span,
            name,
            is_abstract: c.is_abstract,
            super_class,
            type_params: try_opt!(self.validate(&c.type_params)),
            body: c
                .body
                .iter()
                .filter_map(|m| match self.validate(m) {
                    Ok(Some(v)) => Some(Ok(v)),
                    Ok(None) => None,
                    Err(err) => Some(Err(err)),
                })
                .collect::<Result<_, _>>()?,
        })
    }

    fn validate_class_members(
        &mut self,
        c: &Class,
        declare: bool,
    ) -> ValidationResult<Vec<ty::ClassMember>> {
        // Report errors for code like
        //
        //      class C {
        //           foo();
        //      }

        let mut errors = vec![];
        // Span of name
        let mut spans = vec![];
        let mut name: Option<&PropName> = None;

        if !declare {
            for m in &c.body {
                macro_rules! check {
                    ($m:expr, $body:expr) => {{
                        let m = $m;

                        match m.key {
                            PropName::Computed(..) => continue,
                            _ => {}
                        }

                        if $body.is_none() {
                            if name.is_some() && !is_prop_name_eq(&name.unwrap(), &m.key) {
                                for span in replace(&mut spans, vec![]) {
                                    errors.push(Error::TS2391 { span });
                                }
                            }

                            name = Some(&m.key);
                            spans.push(m.key.span());
                        } else {
                            if name.is_none() || is_prop_name_eq(&name.unwrap(), &m.key) {
                                // TODO: Verify parameters

                                spans = vec![];
                                name = None;
                            } else {
                                let constructor_name =
                                    PropName::Ident(Ident::new(js_word!("constructor"), DUMMY_SP));

                                if is_prop_name_eq(&name.unwrap(), &constructor_name) {
                                    for span in replace(&mut spans, vec![]) {
                                        errors.push(Error::TS2391 { span });
                                    }
                                } else if is_prop_name_eq(&m.key, &constructor_name) {
                                    for span in replace(&mut spans, vec![]) {
                                        errors.push(Error::TS2389 { span });
                                    }
                                } else {
                                    spans = vec![];

                                    errors.push(Error::TS2389 { span: m.key.span() });
                                }

                                name = None;
                            }
                        }
                    }};
                }

                match *m {
                    ClassMember::Constructor(ref m) => check!(m, m.body),
                    ClassMember::Method(
                        ref
                        m
                        @
                        ClassMethod {
                            is_abstract: false, ..
                        },
                    ) => check!(m, m.function.body),
                    _ => {}
                }
            }

            // Class definition ended with `foo();`
            for span in replace(&mut spans, vec![]) {
                errors.push(Error::TS2391 { span });
            }
        }

        self.info.errors.extend(errors);

        Ok(vec![])
    }

    #[validator]
    pub(super) fn validate_computed_prop_key(&mut self, span: Span, key: &Expr) {
        let mut errors = vec![];
        let is_symbol_access = match *key {
            Expr::Member(MemberExpr {
                obj:
                    ExprOrSuper::Expr(box Expr::Ident(Ident {
                        sym: js_word!("Symbol"),
                        ..
                    })),
                ..
            }) => true,
            _ => false,
        };

        let ty = match self.validate(&key) {
            Ok(ty) => ty,
            Err(err) => {
                match err {
                    Error::TS2585 { span } => Err(Error::TS2585 { span })?,
                    _ => {}
                }

                errors.push(err);

                Type::any(span)
            }
        };

        match *ty.normalize() {
            Type::Lit(..) => {}
            _ if is_symbol_access => {}
            _ => errors.push(Error::TS1166 { span }),
        }

        if !errors.is_empty() {
            Err(Error::Errors { span, errors })?
        }
    }

    fn validate_inherited_members(&mut self, name: Option<&Ident>, c: &Class, declare: bool) {
        if c.is_abstract || declare {
            return;
        }

        let name_span = name.map(|v| v.span).unwrap_or_else(|| {
            // TODD: c.span().lo() + BytePos(5) (aka class token)
            c.span
        });
        let mut errors = vec![];

        let res: Result<_, Error> = try {
            if let Some(super_class) = &c.super_class {
                let type_args = c
                    .super_type_params
                    .as_ref()
                    .map(|i| self.visit_ts_type_param_instantiation(i));
                let super_ty =
                    self.validate_expr(&super_class, TypeOfMode::RValue, try_opt!(type_args))?;

                match super_ty.normalize() {
                    Type::Class(sc) => {
                        'outer: for sm in &sc.body {
                            match sm {
                                ty::ClassMember::Method(sm) => {
                                    for m in &c.body {
                                        match m {
                                            ClassMember::Method(ref m) => {
                                                if !is_prop_name_eq(&m.key, &sm.key) {
                                                    continue;
                                                }

                                                if m.kind != MethodKind::Method {
                                                    unimplemented!(
                                                        "method property is overriden by \
                                                         non-methof property: {:?}",
                                                        m
                                                    )
                                                }

                                                // TODO: Validate parameters

                                                // TODO: Validate return type
                                                continue 'outer;
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                _ => {
                                    // TODO: Verify
                                    continue 'outer;
                                }
                            }

                            errors.push(Error::TS2515 { span: name_span });

                            if sc.is_abstract {
                                // TODO: Check super class of super class
                            }
                        }
                    }
                    _ => {}
                }
            }
        };

        if let Err(err) = res {
            errors.push(err);
        }

        self.info.errors.extend(errors);
    }
}

/// # Validations
///
///  - TS2515: Validate that class implements all methods.
impl Fold<Class> for Analyzer<'_> {
    fn fold(&mut self, c: Class) -> Class {
        let c = c.fold_children(self);
impl Visit<Class> for Analyzer<'_> {
impl Visit<Class> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &Class) {
        c.visit_children(self);
impl Visit<Class> for Analyzer<'_> {
    fn visit(&mut self, c: &Class) {
        let c = c.visit_children(self);

        self.resolve_parent_interfaces(&c.implements);

        let mut constructor_spans = vec![];
        let mut constructor_required_param_count = None;

        for m in &c.body {
            match *m {
                ClassMember::Constructor(ref cons) => {
                    //
                    if cons.body.is_none() {
                        for p in &cons.params {
                            match *p {
                                PatOrTsParamProp::TsParamProp(..) => {
                                    self.info.errors.push(Error::TS2369 { span: p.span() })
                                }
                                _ => {}
                            }
                        }
                    }

                    {
                        // Check parameter count
                        let required_param_count = cons
                            .params
                            .iter()
                            .filter(|p| match p {
                                PatOrTsParamProp::Pat(Pat::Ident(Ident {
                                    optional: true, ..
                                })) => false,
                                _ => true,
                            })
                            .count();

                        match constructor_required_param_count {
                            Some(v) if required_param_count != v => {
                                for span in constructor_spans.drain(..) {
                                    self.info.errors.push(Error::TS2394 { span })
                                }
                            }

                            None => constructor_required_param_count = Some(required_param_count),
                            _ => {}
                        }
                    }

                    constructor_spans.push(cons.span);
                }

                _ => {}
            }
        }
    }
}

impl Visit<ClassExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &ClassExpr) {
        let ty = match self.validate_type_of_class(c.ident.clone().map(|v| v.sym), &c.class) {
            Ok(ty) => ty.into(),
            Err(err) => {
                self.info.errors.push(err);
                Type::any(c.span())
            }
        };

        let old_this = self.scope.this.take();
        // self.scope.this = Some(ty.clone());

        let c = self.with_child(ScopeKind::Block, Default::default(), |analyzer| {
            if let Some(ref i) = c.ident {
                analyzer.scope.register_type(i.sym.clone(), ty.clone());

                analyzer.validate_inherited_members(None, &c.class, false);
                analyzer.validate_class_members(&c.class, false);

                match analyzer.scope.declare_var(
                    ty.span(),
                    VarDeclKind::Var,
                    i.sym.clone(),
                    Some(ty),
                    // initialized = true
                    true,
                    // declare Class does not allow multiple declarations.
                    false,
                ) {
                    Ok(()) => {}
                    Err(err) => {
                        analyzer.info.errors.push(err);
                    }
                }
            }

            c.visit_children(analyzer)
        });

        self.scope.this = old_this;
    }
}

impl Fold<ClassDecl> for Analyzer<'_> {
    fn fold(&mut self, c: ClassDecl) -> ClassDecl {
        let c: ClassDecl = c.fold_children(self);
impl Visit<ClassDecl> for Analyzer<'_> {
impl Visit<ClassDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &ClassDecl) {
        c.visit_children(self);
impl Visit<ClassDecl> for Analyzer<'_> {
    fn visit(&mut self, c: &ClassDecl) {
        let c: ClassDecl = c.visit_children(self);

        self.validate_inherited_members(Some(&c.ident), &c.class, c.declare);
        self.validate_class_members(&c.class, c.declare);

        let ty = match self.validate_type_of_class(Some(c.ident.sym.clone()), &c.class) {
            Ok(ty) => ty.into(),
            Err(err) => {
                self.info.errors.push(err);
                Type::any(c.span())
            }
        };

        let old_this = self.scope.this.take();
        // self.scope.this = Some(ty.clone());

        self.scope
            .register_type(c.ident.sym.clone(), ty.clone().into());

        match self.scope.declare_var(
            ty.span(),
            VarDeclKind::Var,
            c.ident.sym.clone(),
            Some(ty),
            // initialized = true
            true,
            // declare Class does not allow multiple declarations.
            false,
        ) {
            Ok(()) => {}
            Err(err) => {
                self.info.errors.push(err);
            }
        }

        self.scope.this = old_this;
    }
}

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
