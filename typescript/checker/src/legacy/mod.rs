use self::scope::Scope;
use super::Checker;
use crate::{
    builtin_types::Lib,
    errors::Error,
    loader::Load,
    ty::{self, Alias, ClassInstance, Tuple, Type, TypeParam, TypeRefExt},
    util::{IntoCow, ModuleItemLike, StmtLike},
    Exports, Rule,
};
use fxhash::{FxHashMap, FxHashSet};
use log::debug;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{borrow::Cow, cell::RefCell, path::PathBuf, sync::Arc};
use swc_atoms::{js_word, JsWord};
use swc_common::{
    util::move_map::MoveMap, Fold, FoldWith, Span, Spanned, Visit, VisitWith, DUMMY_SP,
};
use swc_ecma_ast::*;

pub(crate) struct Analyzer<'a, 'b> {
    /// This is false iff it should be treated as error when `1.contains()` is
    /// true
    allow_ref_declaring: bool,
    libs: &'b [Lib],
    rule: Rule,

    /// The code below is valid even when `noImplicitAny` is given.
    ///
    /// ```typescript
    /// var foo: () => [any] = function bar() {}
    /// ```
    span_allowed_implicit_any: Span,

    top_level: bool,
    export_equals_span: Span,

    computed_prop_mode: ComputedPropMode,
}

impl<T> Fold<Vec<T>> for Analyzer<'_, '_>
where
    T: FoldWith<Self> + Send + Sync + StmtLike + ModuleItemLike,
    Vec<T>: FoldWith<Self>
        + for<'any> VisitWith<AmbientFunctionHandler<'any>>
        + for<'any> VisitWith<ImportFinder<'any>>,
{
    fn fold(&mut self, mut items: Vec<T>) -> Vec<T> {
        {
            // We first load imports.
            let mut imports: Vec<ImportInfo> = vec![];

            // Extract imports
            items.visit_with(&mut ImportFinder { to: &mut imports });

            let loader = self.loader;
            let path = self.path.clone();
            let import_results = imports
                .par_iter()
                .map(|import| {
                    loader.load(path.clone(), &*import).map_err(|err| {
                        //
                        (import, err)
                    })
                })
                .collect::<Vec<_>>();

            for res in import_results {
                match res {
                    Ok(import) => {
                        self.resolved_imports.extend(import);
                    }
                    Err((import, mut err)) => {
                        match err {
                            Error::ModuleLoadFailed { ref mut errors, .. } => {
                                self.info.errors.append(errors);
                            }
                            _ => {}
                        }
                        // Mark errored imported types as any to prevent useless errors
                        self.errored_imports.extend(
                            import
                                .items
                                .iter()
                                .map(|&Specifier { ref local, .. }| local.0.clone()),
                        );

                        self.info.push_error(err);
                    }
                }
            }
        }

        if self.top_level {
            let mut has_normal_export = false;
            items = items.move_map(|item| match item.try_into_module_decl() {
                Ok(ModuleDecl::TsExportAssignment(decl)) => {
                    if self.export_equals_span.is_dummy() {
                        self.export_equals_span = decl.span;
                    }
                    if has_normal_export {
                        self.info.push_error(Error::TS2309 { span: decl.span });
                    }

                    //
                    match T::try_from_module_decl(ModuleDecl::TsExportAssignment(decl)) {
                        Ok(v) => v,
                        _ => unreachable!(),
                    }
                }
                Ok(item) => {
                    match item {
                        ModuleDecl::ExportDecl(..)
                        | ModuleDecl::ExportAll(..)
                        | ModuleDecl::ExportDefaultDecl(..)
                        | ModuleDecl::ExportDefaultExpr(..)
                        | ModuleDecl::TsNamespaceExport(..) => {
                            has_normal_export = true;
                            if !self.export_equals_span.is_dummy() {
                                self.info.push_error(Error::TS2309 {
                                    span: self.export_equals_span,
                                });
                            }
                        }
                        _ => {}
                    }

                    match T::try_from_module_decl(item) {
                        Ok(v) => v,
                        _ => unreachable!(),
                    }
                }
                Err(item) => item,
            });
        }

        if !self.in_declare {
            let mut visitor = AmbientFunctionHandler {
                last_ambient_name: None,
                errors: &mut self.info.errors,
            };

            items.visit_with(&mut visitor);

            if visitor.last_ambient_name.is_some() {
                visitor.push_error(Error::TS2391 {
                    span: visitor.last_ambient_name.unwrap().span,
                })
            }
        }

        let items = items.fold_children(self);

        self.handle_pending_exports();

        items
    }
}
