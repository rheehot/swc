use super::Analyzer;
use crate::{
    errors::Error,
    ty::{Enum, Type},
};
use std::convert::TryInto;
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

/// Handles enum.
impl Visit<TsEnumDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, e: &TsEnumDecl) {
        let span = e.span();

        // We don't visit enum variants to allow
        //
        //        const enum E {
        //            a = 10,
        //            b = a,
        //            c = (a+1),
        //            e,
        //            d = ~e,
        //            f = a << 2 >> 1,
        //            g = a << 2 >>> 1,
        //            h = a | b
        //        }

        //        e.visit_children(self);

        self.scope.register_type(
            e.id.sym.clone(),
            match e.clone().try_into() {
                Ok(ty) => ty,
                Err(..) => Type::any(span),
            },
        );

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
        } else {
            e.members = e.members.fold_children(self);
            e.members.visit_children(self);
            e.members = e.members.visit_children(self);
        }
    }
}

impl Analyzer<'_, '_> {
    // Check for constant enum in rvalue.
    pub(super) fn check_rvalue(&mut self, rhs_ty: &Type) {
        match *rhs_ty.normalize() {
            Type::Enum(ref e) if e.is_const => {
                self.info
                    .errors
                    .push(Error::ConstEnumUsedAsVar { span: e.span() });
            }
            _ => {}
        }
    }

    pub(super) fn expand_enum_variant(&self, ty: Type) -> Result<Type, Error> {
        match ty.normalize() {
            Type::EnumVariant(ref v) => {
                if let Some(Type::Enum(Enum { ref members, .. })) =
                    self.scope.types.get(&v.enum_name)
                {
                    if let Some(v) = members.iter().find(|m| match m.id {
                        TsEnumMemberId::Ident(Ident { ref sym, .. })
                        | TsEnumMemberId::Str(Str { value: ref sym, .. }) => *sym == v.name,
                    }) {
                        return Ok(Type::Lit(TsLitType {
                            span: v.span,
                            lit: v.val.clone(),
                        }));
                    }
                }
            }
            _ => {}
        }

        return Ok(ty);
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
            Expr::Unary(..) | Expr::Bin(..) | Expr::Paren(..) => {}

            _ => {
                self.error = true;
                return;
            }
        }
    }
}
