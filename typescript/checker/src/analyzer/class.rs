use super::{scope::ScopeKind, Analyzer};
use crate::{
    analyzer::{ComputedPropMode, VarVisitor, LOG_VISIT},
    errors::Error,
    ty::{Type, TypeRefExt},
    util::EqIgnoreNameAndSpan,
};
use std::mem;
use swc_atoms::js_word;
use swc_common::{Span, Spanned, Visit, VisitWith, DUMMY_SP};
use swc_ecma_ast::*;

impl Visit<Class> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &Class) {
        if LOG_VISIT {
            println!("Visit<Class>");
        }

        c.visit_children(self);

        self.validate_parent_interfaces(&c.implements);

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
                    } else {
                        // TODO: Check parameter count
                    }
                }

                _ => {}
            }
        }
    }
}

impl Analyzer<'_, '_> {
    fn validate_class_members(&mut self, c: &Class, declare: bool) {
        fn is_prop_name_eq(l: &PropName, r: &PropName) -> bool {
            macro_rules! check {
                ($l:expr, $r:expr) => {{
                    let l = $l;
                    let r = $r;

                    match l {
                        PropName::Ident(Ident { ref sym, .. })
                        | PropName::Str(Str { value: ref sym, .. }) => match *r {
                            PropName::Ident(Ident { sym: ref r_sym, .. })
                            | PropName::Str(Str {
                                value: ref r_sym, ..
                            }) => return sym == r_sym,
                            PropName::Num(n) => return sym == &*n.value.to_string(),
                            _ => return false,
                        },
                        PropName::Computed(..) => return false,
                        _ => {}
                    }
                }};
            }

            check!(l, r);
            check!(r, l);

            false
        }

        // Report errors for code like
        //
        //      class C {
        //           foo();
        //      }

        let mut errors = vec![];
        // Span of name
        let mut spans = vec![];
        let mut name: Option<&PropName> = None;

        for m in &c.body {
            macro_rules! check {
                ($m:expr, $body:expr) => {{
                    if declare {
                        continue;
                    }

                    let m = $m;

                    match m.key {
                        PropName::Computed(..) => continue,
                        _ => {}
                    }

                    if $body.is_none() {
                        if name.is_some() && !is_prop_name_eq(&name.unwrap(), &m.key) {
                            for span in mem::replace(&mut spans, vec![]) {
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
                                for span in mem::replace(&mut spans, vec![]) {
                                    errors.push(Error::TS2391 { span });
                                }
                            } else if is_prop_name_eq(&m.key, &constructor_name) {
                                for span in mem::replace(&mut spans, vec![]) {
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
                ClassMember::Method(ref m) => check!(m, m.function.body),
                _ => {}
            }
        }

        if !declare {
            // Class definition ended with `foo();`
            for span in mem::replace(&mut spans, vec![]) {
                errors.push(Error::TS2391 { span });
            }
        }

        self.info.errors.extend(errors);
    }

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

        analyze!(self, {
            let ty = match self.type_of(&key) {
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
        });
    }
}

impl Visit<ClassMember> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &ClassMember) {
        if LOG_VISIT {
            println!("Visit<ClassMember>");
        }

        self.computed_prop_mode = ComputedPropMode::Class {
            has_body: match *node {
                ClassMember::Method(ClassMethod { ref function, .. }) => function.body.is_some(),
                _ => false,
            },
        };

        node.visit_children(self);
    }
}

impl Visit<ClassProp> for Analyzer<'_, '_> {
    fn visit(&mut self, p: &ClassProp) {
        if LOG_VISIT {
            println!("Visit<ClassProp>");
        }

        p.visit_children(self);

        // Verify key if key is computed
        if p.computed {
            self.validate_computed_prop_key(p.span, &p.key);
        }

        if let Some(ref ty) = p.type_ann {
            let span = ty.span();
            analyze!(self, {
                let ty: Type = ty.type_ann.clone().into();
                self.expand_type(span, ty.owned())?;
            });
        }

        if let Some(ref value) = p.value {
            analyze!(self, {
                self.type_of(&value)?;
            });
        }

        self.scope.declaring_prop = None;
    }
}

impl Visit<ClassExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &ClassExpr) {
        if LOG_VISIT {
            println!("Visit<ClassExpr>");
        }

        let ty = match self.validate_type_of_class(c.ident.clone().map(|v| v.sym), &c.class) {
            Ok(ty) => ty,
            Err(err) => {
                self.info.errors.push(err);
                Type::any(c.span()).into()
            }
        };

        self.scope.this = Some(ty.clone());

        self.with_child(ScopeKind::Block, Default::default(), |analyzer| {
            if let Some(ref i) = c.ident {
                analyzer.scope.register_type(i.sym.clone(), ty.clone());

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

            c.visit_children(analyzer);
        });

        self.scope.this = None;
    }
}

impl Visit<ClassDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &ClassDecl) {
        c.visit_children(self);

        self.validate_class_members(&c.class, c.declare);

        let ty = match self.validate_type_of_class(Some(c.ident.sym.clone()), &c.class) {
            Ok(ty) => ty,
            Err(err) => {
                self.info.errors.push(err);
                Type::any(c.span()).into()
            }
        };

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

        self.scope.this = None;
    }
}

impl Visit<ClassMethod> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &ClassMethod) {
        println!("Visit<ClassMethod>");

        let entry = self.with_child(ScopeKind::Fn, Default::default(), |child| {
            child.return_type_span = c.span();

            let old = child.allow_ref_declaring;
            child.allow_ref_declaring = false;

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

                debug_assert_eq!(child.allow_ref_declaring, false);

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
            c.key.visit_with(child);
            c.function.visit_children(child);

            debug_assert_eq!(child.allow_ref_declaring, false);
            child.allow_ref_declaring = old;

            child
                .inferred_return_types
                .get_mut()
                .remove_entry(&c.span())
                .unwrap_or_default()
        });

        if c.kind == MethodKind::Getter && c.function.body.is_some() {
            // getter property must have return statements.
            if entry.1.is_empty() {
                self.info
                    .errors
                    .push(Error::GetterPropWithoutReturn { span: c.key.span() });
            }
        }

        *self
            .inferred_return_types
            .get_mut()
            .entry(c.span())
            .or_default() = entry.1;
    }
}

impl Visit<TsIndexSignature> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &TsIndexSignature) {
        node.visit_children(self);
    }
}

impl Visit<Constructor> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &Constructor) {
        self.with_child(ScopeKind::Fn, Default::default(), |child| {
            child.return_type_span = c.span();

            let old = child.allow_ref_declaring;
            child.allow_ref_declaring = false;

            {
                // Validate params
                // TODO: Move this to parser
                let mut has_optional = false;
                for p in &c.params {
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

            c.params.iter().for_each(|param| {
                let mut names = vec![];

                let mut visitor = VarVisitor { names: &mut names };

                param.visit_with(&mut visitor);

                child.declaring.extend_from_slice(&names);

                debug_assert_eq!(child.allow_ref_declaring, false);

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
                    child.declaring.remove_item(&n).unwrap();
                }
            });

            child.inferred_return_types.get_mut().insert(c.span, vec![]);
            c.visit_children(child);

            debug_assert_eq!(child.allow_ref_declaring, false);
            child.allow_ref_declaring = old;
        });
    }
}
