use super::Analyzer;
use crate::errors::Error;
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Visit<TsEnumDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, e: &TsEnumDecl) {
        e.visit_children(self);

        self.scope.register_type(e.id.sym.clone(), e.clone().into());

        // Validate const enums
        if e.is_const {
            for m in &e.members {
                if let Some(ref init) = m.init {
                    let mut v = LitValidator {
                        error: false,
                        decl: &e,
                    };
                    init.visit_with(&mut v);
                    if v.error {
                        self.info
                            .errors
                            .push(Error::InvalidInitInConstEnum { span: init.span() })
                    }
                }
            }
        }
    }
}

struct LitValidator<'a> {
    decl: &'a TsEnumDecl,
    error: bool,
}

impl Visit<Expr> for LitValidator<'_> {
    fn visit(&mut self, e: &Expr) {
        e.visit_children(self);

        match e {
            Expr::Lit(..) => {}
            Expr::Ident(ref i) => {
                let is_ref = self.decl.members.iter().any(|m| match m.id {
                    TsEnumMemberId::Ident(Ident { ref sym, .. })
                    | TsEnumMemberId::Str(Str { value: ref sym, .. }) => *sym == i.sym,
                });
                if !is_ref {
                    self.error = true;
                    return;
                }
            }

            _ => {
                self.error = true;
                return;
            }
        }
    }
}
