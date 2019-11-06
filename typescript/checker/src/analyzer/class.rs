use super::{scope::ScopeKind, Analyzer};
use crate::{errors::Error, ty::Type};
use swc_common::{Span, Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<Class> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &Class) {
        self.validate_parent_interfaces(&c.implements);

        c.visit_children(self);
    }
}

impl Analyzer<'_, '_> {
    pub(super) fn validate_computed_prop_key(&mut self, span: Span, key: &Expr) {
        analyze!(self, {
            let mut errors = vec![];
            let ty = match self.type_of(&key) {
                Ok(ty) => ty,
                Err(err) => {
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

        c.visit_children(self);

        self.scope.this = None;
    }
}
