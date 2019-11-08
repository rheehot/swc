use super::{scope::ScopeKind, Analyzer};
use crate::{analyzer::ComputedPropMode, errors::Error, ty::Type, util::EqIgnoreNameAndSpan};
use std::mem;
use swc_common::{Span, Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<Class> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &Class) {
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
                        println!("cons.body: {:?}", cons.body);
                    }
                }

                _ => {}
            }
        }
    }
}

impl Analyzer<'_, '_> {
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

        for m in &c.body {
            match *m {
                ClassMember::Method(ref m) => {
                    match m.key {
                        PropName::Computed(..) => continue,
                        _ => {}
                    }

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

                    if m.function.body.is_none() {
                        if name.is_some() && !is_prop_name_eq(&name.unwrap(), &m.key) {
                            if !declare {
                                for span in mem::replace(&mut spans, vec![]) {
                                    errors.push(Error::TS2391 { span });
                                }
                            }
                        } else {
                            spans.push(m.key.span());
                            name = Some(&m.key)
                        }
                    } else {
                        if name.is_none() || is_prop_name_eq(&name.unwrap(), &m.key) {
                            // TODO: Verify parameters

                            spans = vec![];
                            name = None;
                        } else {
                            if !declare {
                                for span in mem::replace(&mut spans, vec![]) {
                                    errors.push(Error::TS2391 { span });
                                }
                            }
                        }
                    }
                }
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
        analyze!(self, {
            let mut errors = vec![];
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
        match *p.key {
            Expr::Ident(Ident { ref sym, .. }) => self.scope.declaring_prop = Some(sym.clone()),
            _ => {}
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
    fn visit(&mut self, n: &ClassMethod) {
        if n.kind == MethodKind::Getter {
            let entry = self.with_child(ScopeKind::Fn, Default::default(), |child| {
                child.return_type_span = n.span();

                child
                    .inferred_return_types
                    .get_mut()
                    .insert(n.span(), Default::default());

                n.key.visit_with(child);
                n.function.visit_children(child);

                child
                    .inferred_return_types
                    .get_mut()
                    .remove_entry(&n.span())
                    .unwrap_or_default()
            });

            if entry.1.is_empty() {
                // getter property must have return statements.
                self.info
                    .errors
                    .push(Error::GetterPropWithoutReturn { span: n.key.span() });
            }

            *self
                .inferred_return_types
                .get_mut()
                .entry(n.span())
                .or_default() = entry.1;
        } else {
            n.visit_children(self)
        }
    }
}

impl Visit<TsIndexSignature> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &TsIndexSignature) {
        node.visit_children(self);
    }
}
