use super::Analyzer;
use crate::{
    analyzer::util::ResultExt,
    errors::Error,
    ty::Type,
    validator::{Validate, ValidateWith},
};
use std::mem::replace;
use swc_atoms::{js_word, JsWord};
use swc_common::{Fold, FoldWith, Span, Spanned};
use swc_common::{Fold, FoldWith, Span, Spanned, Visit, VisitWith};
use swc_common::{Span, Spanned, Visit, VisitWith};
use swc_common::{Fold, FoldWith, Span, Spanned, Visit};
use swc_ecma_ast::*;

// ModuleDecl::ExportNamed(export) => {}
//
// ModuleDecl::ExportAll(export) => unimplemented!("export * from
// 'other-file';"),
//
// ModuleDecl::TsNamespaceExport(ns) =>
// unimplemented!("export namespace"),

impl Analyzer<'_, '_> {
    /// This methods exports unresolved expressions, which depends on
    /// expressions that comes after the expression.
    pub(super) fn handle_pending_exports(&mut self) {
        if self.pending_exports.is_empty() {
            return;
        }

        let pending_exports: Vec<_> = replace(&mut self.pending_exports, Default::default());

        for ((sym, _), expr) in pending_exports {
            // TODO: Allow multiple exports with same name.

            debug_assert_eq!(self.info.exports.vars.get(&sym), None);

            let exported_sym = if *sym != js_word!("default") {
                Some(&sym)
            } else {
                match expr {
                    Expr::Ident(ref i) => Some(&i.sym),
                    _ => None,
                }
            };
            let ty = match exported_sym
                .and_then(|exported_sym| self.scope.types.remove(&exported_sym))
            {
                Some(export) => export,
                None => match expr.validate_with(self) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.info.push_error(err);
                        return;
                    }
                },
            };
            self.info.exports.vars.insert(sym, ty.freeze());
        }

        assert_eq!(self.pending_exports, vec![]);
    }

    pub(super) fn export_default_expr(&mut self, expr: &Expr) {
        assert_eq!(
            self.info.exports.vars.get(&js_word!("default")),
            None,
            "A module can export only one item as default"
        );

        let ty = match self.validate(expr) {
            Ok(ty) => ty,
            Err(err) => {
                match err {
                    // Handle hoisting. This allows
                    //
                    // export = React
                    // declare namespace React {}
                    Error::UndefinedSymbol { .. } => {
                        self.pending_exports
                            .push(((js_word!("default"), expr.span()), expr.clone()));
                        return;
                    }
                    _ => {}
                }
                self.info.push_error(err);
                return;
            }
        };
        self.info
            .exports
            .vars
            .insert(js_word!("default"), ty.freeze());
    }
}

impl Visit<ExportDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, export: &ExportDecl) {
        export.visit_children(self);

        match export.decl {
            Decl::Fn(ref f) => self.export(f.span(), f.ident.sym.clone(), None),
            Decl::TsInterface(ref i) => self.export(i.span(), i.id.sym.clone(), None),
            Decl::Class(ref c) => self.export(c.span(), c.ident.sym.clone(), None),
            Decl::Var(ref var) => {
                // unimplemented!("export var Foo = a;")
                for decl in &var.decls {
                    let res = self.declare_vars_inner(var.kind, &decl.name, true);
                    match res {
                        Ok(..) => {}
                        Err(err) => self.info.push_error(err),
                    }
                }
            }
            Decl::TsEnum(ref e) => {
                // TODO: Allow multiple exports with same name.
                debug_assert_eq!(self.info.exports.types.get(&e.id.sym), None);

                let ty = e
                    .validate_with(self)
                    .store(&mut self.info.errors)
                    .map(Type::from);

                self.info.exports.types.insert(
                    e.id.sym.clone(),
                    {
                        let span = e.span();
                        ty.unwrap_or_else(|| Type::any(span))
                    }
                    .freeze(),
                );
            }
            Decl::TsModule(..) => unimplemented!("export module "),
            Decl::TsTypeAlias(ref decl) => {
                // export type Foo = 'a' | 'b';
                // export type Foo = {};

                // TODO: Handle type parameters.

                self.export(decl.span, decl.id.sym.clone(), None)
            }
        }
    }
}

impl Fold<ExportDefaultDecl> for Analyzer<'_> {
    fn fold(&mut self, export: ExportDefaultDecl) -> ExportDefaultDecl {
        let export = export.fold_children(self);
impl Visit<ExportDefaultDecl> for Analyzer<'_> {
impl Visit<ExportDefaultDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, export: &ExportDefaultDecl) {
        export.visit_children(self);
impl Visit<ExportDefaultDecl> for Analyzer<'_> {
    fn visit(&mut self, export: &ExportDefaultDecl) {
        let export = export.visit_children(self);

        match export.decl {
            DefaultDecl::Fn(ref f) => {
                let i = f
                    .ident
                    .as_ref()
                    .map(|v| v.sym.clone())
                    .unwrap_or(js_word!("default"));
                let fn_ty = match f.function.validate_with(self) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.info.push_error(err);
                        return;
                    }
                };
                self.register_type(i.clone(), fn_ty.into())
                    .store(&mut self.info.errors);
                self.export(f.span(), js_word!("default"), Some(i))
            }
            DefaultDecl::Class(..) => unimplemented!("export default class"),
            DefaultDecl::TsInterfaceDecl(ref i) => {
                self.export(i.span(), js_word!("default"), Some(i.id.sym.clone()))
            }
        };
    }
}

impl Analyzer<'_, '_> {
    /// Exports a type.
    ///
    /// `scope.regsiter_type` should be called before calling this method.
    fn export(&mut self, span: Span, name: JsWord, from: Option<JsWord>) {
        let from = from.unwrap_or_else(|| name.clone());

        let ty = match self.scope.find_type(&from) {
            Some(ty) => ty,
            None => {
                self.info.push_error(Error::UndefinedSymbol { span });
                return;
            }
        };

        // TODO: Change this to error.
        assert_eq!(self.info.exports.types.get(&name), None);
        self.info.exports.types.insert(name, ty.clone().freeze());
    }

    /// Exports a varaible.
    fn export_expr(&mut self, name: JsWord, e: &Expr) {
        unimplemented!("export_expr")
    }
}

/// Done
impl Visit<TsExportAssignment> for Analyzer<'_, '_> {
    fn visit(&mut self, s: &TsExportAssignment) {
        self.export_expr(js_word!("default"), &s.expr);
    }
}

/// Done
impl Visit<ExportDefaultExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, s: &ExportDefaultExpr) {
        self.export_expr(js_word!("default"), &s.expr);
    }
}
