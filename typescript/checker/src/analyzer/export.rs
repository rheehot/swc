use super::Analyzer;
use crate::{
    errors::Error,
    ty::{Type, TypeRefExt},
};
use std::{convert::TryInto, mem, sync::Arc};
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

impl Analyzer<'_> {
    pub(super) fn handle_pending_exports(&mut self) {
        if self.pending_exports.is_empty() {
            return;
        }

        let pending_exports = mem::replace(&mut self.pending_exports, Default::default());

        for ((sym, _), expr) in pending_exports {
            // TODO: Allow multiple exports with same name.

            debug_assert_eq!(self.info.exports.types.get(&sym), None);

            let exported_sym = if *sym != js_word!("default") {
                Some(&sym)
            } else {
                match *expr {
                    Expr::Ident(ref i) => Some(&i.sym),
                    _ => None,
                }
            };
            let ty = match exported_sym
                .and_then(|exported_sym| self.scope.types.remove(&exported_sym))
            {
                Some(export) => export,
                None => match self.validate_expr(&expr) {
                    Ok(ty) => ty.to_static(),
                    Err(err) => {
                        self.info.errors.push(err);
                        return;
                    }
                },
            };
            self.info.exports.types.insert(sym, Arc::new(ty));
        }

        assert_eq!(self.pending_exports, vec![]);
    }

    pub(super) fn export_default_expr(&mut self, expr: &Expr) {
        assert_eq!(
            self.info.exports.get(&js_word!("default")),
            None,
            "A module can export only one item as default"
        );

        let ty = match self.validate_expr(expr) {
            Ok(ty) => ty.to_static(),
            Err(err) => {
                match err {
                    // Handle hoisting. This allows
                    //
                    // export = React
                    // declare namespace React {}
                    Error::UndefinedSymbol { .. } => {
                        self.pending_exports
                            .push(((js_word!("default"), expr.span()), box expr.clone()));
                        return;
                    }
                    _ => {}
                }
                self.info.errors.push(err);
                return;
            }
        };
        self.info.exports.insert(js_word!("default"), Arc::new(ty));
    }
}

impl Visit<ExportDecl> for Analyzer<'_> {
    fn visit(&mut self, export: &ExportDecl) {
        let export = export.visit_children(self);

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
                        Err(err) => self.info.errors.push(err),
                    }
                }
            }
            Decl::TsEnum(ref e) => {
                // TODO: Allow multiple exports with same name.
                debug_assert_eq!(self.info.exports.get(&e.id.sym), None);

                self.info.exports.insert(
                    e.id.sym.clone(),
                    Arc::new({
                        let span = e.span();
                        match e.clone().try_into() {
                            Ok(ty) => ty,
                            Err(e) => Type::any(span),
                        }
                    }),
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

        export
    }
}

impl Fold<ExportDefaultDecl> for Analyzer<'_> {
    fn fold(&mut self, export: ExportDefaultDecl) -> ExportDefaultDecl {
        let export = export.fold_children(self);
impl Visit<ExportDefaultDecl> for Analyzer<'_> {
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
                let fn_ty = match self.type_of_fn(&f.function) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.info.errors.push(err);
                        return export;
                    }
                };
                self.scope.register_type(i.clone(), fn_ty);
                self.export(f.span(), js_word!("default"), Some(i))
            }
            DefaultDecl::Class(..) => unimplemented!("export default class"),
            DefaultDecl::TsInterfaceDecl(ref i) => {
                self.export(i.span(), js_word!("default"), Some(i.id.sym.clone()))
            }
        };
    }
}

impl Analyzer<'_> {
    /// `scope.regsiter_type` should be called before calling this method.
    fn export(&mut self, span: Span, name: JsWord, from: Option<JsWord>) {
        let from = from.unwrap_or_else(|| name.clone());

        let ty = match self.scope.find_type(&from) {
            Some(ty) => ty.to_static(),
            None => {
                self.info.errors.push(Error::UndefinedSymbol { span });
                return;
            }
        };

        // TODO: Change this to error.
        assert_eq!(self.info.exports.get(&name), None);
        self.info.exports.insert(name, Arc::new(ty));
    }
}

impl Visit<TsExportAssignment> for Analyzer<'_> {
    fn visit(&mut self, s: &TsExportAssignment) {
        let ty = self.validate_expr(&s.expr)?;

        self.export_expr(js_word!("default"), ty);
    }
}

impl Visit<ExportDefaultExpr> for Analyzer<'_> {
    fn visit(&mut self, s: &ExportDefaultExpr) {
        let ty = self.validate_expr(&s.expr)?;

        self.export_expr(js_word!("default"), ty);
    }
}
