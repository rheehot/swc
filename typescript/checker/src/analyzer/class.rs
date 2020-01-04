use super::{
    expr::TypeOfMode,
    scope::ScopeKind,
    util::{is_prop_name_eq, VarVisitor},
    Analyzer,
};
use crate::{analyzer::util::ResultExt, errors::Error, swc_common::VisitWith, ty, ty::Type};
use std::mem::replace;
use swc_atoms::js_word;
use swc_common::{util::move_map::MoveMap, Fold, Span, Spanned, DUMMY_SP};
use swc_common::{util::move_map::MoveMap, Span, Spanned, Visit, DUMMY_SP};
use swc_common::{util::move_map::MoveMap, Fold, Span, Spanned, Visit, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ts_checker_macros::validator;

impl Analyzer<'_> {
    fn validate_class_property(&mut self, p: &ClassProp) -> Result<(), Error> {
        // TODO: children

        let mut errors = vec![];

        // Verify key if key is computed
        if p.computed {
            self.validate_computed_prop_key(p.span, &p.key);
        }

        if let Some(ref ty) = p.type_ann {
            let span = ty.span();

            let ty: Type = ty.type_ann.clone().into();
            self.expand_type(span, ty.owned()).store(&mut errors);
        }

        if let Some(ref value) = p.value {
            self.validate_expr(&value).store(&mut errors);
        }

        self.info.errors.extend(errors);

        Ok(())
    }

    fn validate_constructor(&mut self, c: &Constructor) -> Result<(), Error> {
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

            let params = params.move_map(|param| {
                let mut names = vec![];

                let mut visitor = VarVisitor { names: &mut names };

                param.visit_with(&mut visitor);

                child.declaring.extend(names.clone());

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
                            let ty = i.type_ann.clone().map(Type::from);
                            let ty = match ty {
                                Some(ty) => match child.expand_type(i.span, ty.owned()) {
                                    Ok(ty) => Some(ty.into_owned().into_static()),
                                    Err(err) => {
                                        child.info.errors.push(err);
                                        Some(Type::any(i.span))
                                    }
                                },
                                None => None,
                            };

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

                for n in names {
                    let idx = child
                        .declaring
                        .iter()
                        .rposition(|name| n == name)
                        .expect("failed to find inserted name");
                    child.declaring.remove(idx).unwrap();
                }

                param
            });

            child.inferred_return_types.get_mut().insert(c_span, vec![]);
            let c = Constructor { params, ..c }.fold_children(child);
            let c = Constructor { params, ..c }.visit_children(child);

            Ok(())
        })
    }

    fn validate_class_method(&mut self, c: &ClassMethod) -> Result<(), Error> {
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

                child.declaring.extend_from_slice(&names);

                match child.declare_vars(VarDeclKind::Let, pat) {
                    Ok(()) => {}
                    Err(err) => {
                        child.info.errors.push(err);
                    }
                }

                for n in names {
                    child.declaring.remove_item(&n).unwrap();
                }
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
            // getter property must have return statements.
            if entry.1.is_empty() {
                self.info
                    .errors
                    .push(Error::GetterPropWithoutReturn { span: key_span });
            }
        }

        Ok(())
    }

    fn validate_class_members(&mut self, c: &Class, declare: bool) {
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

        let ty = match self.validate_expr(&key) {
            Ok(ty) => ty,
            Err(err) => {
                match err {
                    Error::TS2585 { span } => Err(Error::TS2585 { span })?,
                    _ => {}
                }

                errors.push(err);

                Type::any(span).owned()
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
                let super_ty = self.type_of_expr(
                    &super_class,
                    TypeOfMode::RValue,
                    c.super_type_params.as_ref(),
                )?;

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

impl Visit<ClassExpr> for Analyzer<'_> {
    fn visit(&mut self, c: &ClassExpr) {
        let ty = match self.validate_type_of_class(c.ident.clone().map(|v| v.sym), &c.class) {
            Ok(ty) => ty,
            Err(err) => {
                self.info.errors.push(err);
                Type::any(c.span()).into()
            }
        };

        let old_this = self.scope.this.take();
        self.scope.this = Some(ty.clone());

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
    fn visit(&mut self, c: &ClassDecl) {
        c.visit_children(self);
impl Visit<ClassDecl> for Analyzer<'_> {
    fn visit(&mut self, c: &ClassDecl) {
        let c: ClassDecl = c.visit_children(self);

        self.validate_inherited_members(Some(&c.ident), &c.class, c.declare);
        self.validate_class_members(&c.class, c.declare);

        let ty = match self.validate_type_of_class(Some(c.ident.sym.clone()), &c.class) {
            Ok(ty) => ty,
            Err(err) => {
                self.info.errors.push(err);
                Type::any(c.span()).into()
            }
        };

        let old_this = self.scope.this.take();
        self.scope.this = Some(ty.clone());

        self.scope.register_type(c.ident.sym.clone(), ty.clone());

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
