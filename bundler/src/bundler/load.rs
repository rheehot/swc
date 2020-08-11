use super::{export::Exports, helpers::Helpers, Bundler};
use crate::{
    bundler::{export::RawExports, import::RawImports},
    id::{Id, ModuleId},
    util,
    util::IntoParallelIterator,
    Load, Resolve,
};
use anyhow::{Context, Error};
use is_macro::Is;
#[cfg(feature = "rayon")]
use rayon::iter::ParallelIterator;
use swc_atoms::js_word;
use swc_common::{sync::Lrc, FileName, Mark, SourceFile, DUMMY_SP};
use swc_ecma_ast::{
    Expr, ExprOrSuper, ImportDecl, ImportSpecifier, Invalid, MemberExpr, Module, ModuleDecl, Str,
};
use swc_ecma_transforms::resolver_with_mark;
use swc_ecma_visit::{FoldWith, Node, Visit, VisitWith};
/// Module after applying transformations.
#[derive(Debug, Clone)]
pub(super) struct TransformedModule {
    pub id: ModuleId,
    pub fm: Lrc<SourceFile>,
    pub module: Lrc<Module>,
    pub imports: Lrc<Imports>,
    pub exports: Lrc<Exports>,

    /// If false, the module will be wrapped with a small helper function.
    pub is_es6: bool,

    /// Used helpers
    pub helpers: Lrc<Helpers>,

    mark: Mark,
}

impl TransformedModule {
    /// THe marker for the module's top-level identifiers.
    pub fn mark(&self) -> Mark {
        self.mark
    }
}

impl<L, R> Bundler<'_, L, R>
where
    L: Load,
    R: Resolve,
{
    /// Phase 1 (discovery)
    ///
    /// We apply transforms at this phase to make cache efficient.
    /// As we cache in this phase, changing dependency does not affect cache.
    pub(super) fn load_transformed(
        &self,
        file_name: &FileName,
    ) -> Result<Option<TransformedModule>, Error> {
        self.run(|| {
            log::trace!("load_transformed: ({})", file_name);

            // In case of common module
            if let Some(cached) = self.scope.get_module_by_path(&file_name) {
                log::info!("Cached: {}", file_name);
                return Ok(Some(cached));
            }

            let (_, fm, module) = self.load(&file_name).context("Bundler.load() failed")?;
            let (v, files) = self
                .analyze(&file_name, fm.clone(), module)
                .context("failed to analyze module")?;

            log::info!("Storing module: {}", file_name);
            self.scope.store_module(v.clone());

            // Load dependencies and store them in the `Scope`
            let results = files
                .into_par_iter()
                .map(|source| self.resolve(file_name, &source.src.value))
                .map(|path| self.load_transformed(&*path?))
                .collect::<Vec<_>>();

            // Do tasks in parallel, and then wait for result
            for result in results {
                result?;
            }

            Ok(Some(v))
        })
    }

    fn load(&self, file_name: &FileName) -> Result<(ModuleId, Lrc<SourceFile>, Module), Error> {
        self.run(|| {
            let (module_id, _) = self.scope.module_id_gen.gen(file_name);

            let (fm, module) = self
                .loader
                .load(&file_name)
                .with_context(|| format!("Bundler.loader.load({}) failed", file_name))?;
            self.scope.mark_as_loaded(module_id);
            Ok((module_id, fm, module))
        })
    }

    /// This methods returns [Source]s which should be loaded.
    fn analyze(
        &self,
        file_name: &FileName,
        fm: Lrc<SourceFile>,
        mut module: Module,
    ) -> Result<(TransformedModule, Vec<Source>), Error> {
        self.run(|| {
            log::trace!("transform_module({})", fm.name);
            module = module.fold_with(&mut resolver_with_mark(self.top_level_mark));

            let (id, mark) = self.scope.module_id_gen.gen(file_name);

            // {
            //     let code = self
            //         .swc
            //         .print(
            //             &module.clone().fold_with(&mut HygieneVisualizer),
            //             SourceMapsConfig::Bool(false),
            //             None,
            //             false,
            //         )
            //         .unwrap()
            //         .code;
            //
            //     println!("Resolved:\n{}\n\n", code);
            // }

            let imports = self.extract_import_info(file_name, &mut module, mark);

            // {
            //     let code = self
            //         .swc
            //         .print(
            //             &module.clone().fold_with(&mut HygieneVisualizer),
            //             SourceMapsConfig::Bool(false),
            //             None,
            //             false,
            //         )
            //         .unwrap()
            //         .code;
            //
            //     println!("After imports:\n{}\n", code,);
            // }

            let exports = self.extract_export_info(&module);

            let is_es6 = {
                let mut v = Es6ModuleDetector {
                    forced_es6: false,
                    found_other: false,
                };
                module.visit_with(&Invalid { span: DUMMY_SP } as _, &mut v);
                v.forced_es6 || !v.found_other
            };
            if is_es6 {
                module = self.drop_unused(module, None);
            }

            let (imports, exports) = util::join(
                || self.resolve_imports(file_name, imports),
                || self.resolve_exports(file_name, exports),
            );
            let (imports, mut import_files) = imports?;
            let (exports, reexport_files) = exports?;
            import_files.extend(reexport_files);

            let module = Lrc::new(module);

            Ok((
                TransformedModule {
                    id,
                    fm,
                    module,
                    imports: Lrc::new(imports),
                    exports: Lrc::new(exports),
                    is_es6,
                    helpers: Default::default(),
                    mark,
                },
                import_files,
            ))
        })
    }

    /// Resolve re-exports.
    fn resolve_exports(
        &self,
        base: &FileName,
        raw: RawExports,
    ) -> Result<(Exports, Vec<Source>), Error> {
        self.run(|| {
            log::trace!("resolve_exports({})", base);
            let mut files = vec![];

            let mut exports = Exports::default();

            let items = raw
                .items
                .into_par_iter()
                .map(|(src, ss)| -> Result<_, Error> {
                    let info = match src {
                        Some(src) => {
                            let path = self.resolve(base, &src.value)?;
                            let (id, _) = self.scope.module_id_gen.gen(&path);
                            Some((id, src))
                        }
                        None => None,
                    };

                    Ok((info, ss))
                })
                .collect::<Vec<_>>();

            for res in items {
                let (info, specifiers) = res?;

                match info {
                    None => exports.items.extend(specifiers),
                    Some((id, src)) => {
                        //
                        let src = Source {
                            is_loaded_synchronously: true,
                            is_unconditional: false,
                            module_id: id,
                            src,
                        };
                        exports
                            .reexports
                            .entry(src.clone())
                            .or_default()
                            .extend(specifiers);
                        files.push(src);
                    }
                }
            }

            Ok((exports, files))
        })
    }

    /// Resolve dependencies
    fn resolve_imports(
        &self,
        base: &FileName,
        info: RawImports,
    ) -> Result<(Imports, Vec<Source>), Error> {
        self.run(|| {
            log::trace!("resolve_imports({})", base);
            let mut files = vec![];

            let mut merged = Imports::default();
            let RawImports {
                imports,
                lazy_imports,
                dynamic_imports,
            } = info;

            let loaded = imports
                .into_par_iter()
                .map(|v| (v, false, true))
                .chain(lazy_imports.into_par_iter().map(|v| (v, false, false)))
                .chain(dynamic_imports.into_par_iter().map(|src| {
                    (
                        ImportDecl {
                            span: src.span,
                            specifiers: vec![],
                            src,
                            type_only: false,
                        },
                        true,
                        false,
                    )
                }))
                .map(|(decl, dynamic, unconditional)| -> Result<_, Error> {
                    //
                    let file_name = self.resolve(base, &decl.src.value)?;
                    let (id, _) = self.scope.module_id_gen.gen(&file_name);

                    Ok((id, decl, dynamic, unconditional))
                })
                .collect::<Vec<_>>();

            for res in loaded {
                // TODO: Report error and proceed instead of returning an error
                let (id, decl, is_dynamic, is_unconditional) = res?;

                let src = Source {
                    is_loaded_synchronously: !is_dynamic,
                    is_unconditional,
                    module_id: id,
                    src: decl.src,
                };
                files.push(src.clone());

                // TODO: Handle rename
                let mut specifiers = vec![];
                for s in decl.specifiers {
                    match s {
                        ImportSpecifier::Named(s) => specifiers.push(Specifier::Specific {
                            local: s.local.into(),
                            alias: s.imported.map(From::from),
                        }),
                        ImportSpecifier::Default(s) => specifiers.push(Specifier::Specific {
                            local: s.local.into(),
                            alias: Some(Id::new(js_word!("default"), s.span.ctxt())),
                        }),
                        ImportSpecifier::Namespace(s) => {
                            specifiers.push(Specifier::Namespace {
                                local: s.local.into(),
                            });
                        }
                    }
                }

                merged.specifiers.push((src, specifiers));
            }

            Ok((merged, files))
        })
    }
}

#[derive(Debug, Default)]
pub(super) struct Imports {
    /// If imported ids are empty, it is a side-effect import.
    pub specifiers: Vec<(Source, Vec<Specifier>)>,
}

/// Clone is relatively cheap
#[derive(Debug, Clone, Is)]
pub(super) enum Specifier {
    Specific { local: Id, alias: Option<Id> },
    Namespace { local: Id },
}

impl Specifier {
    pub fn local(&self) -> &Id {
        match self {
            Specifier::Specific { local, .. } => local,
            Specifier::Namespace { local, .. } => local,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct Source {
    pub is_loaded_synchronously: bool,
    pub is_unconditional: bool,

    pub module_id: ModuleId,
    // Clone is relatively cheap, thanks to string_cache.
    pub src: Str,
}

struct Es6ModuleDetector {
    /// If import statement or export is detected, it's an es6 module regardless
    /// of other codes.
    forced_es6: bool,
    /// True if other module system is detected.
    found_other: bool,
}

impl Visit for Es6ModuleDetector {
    fn visit_member_expr(&mut self, e: &MemberExpr, _: &dyn Node) {
        e.obj.visit_with(e as _, self);

        if e.computed {
            e.prop.visit_with(e as _, self);
        }

        match &e.obj {
            ExprOrSuper::Expr(e) => {
                match &**e {
                    Expr::Ident(i) => {
                        // TODO: Check syntax context (Check if marker is the global mark)
                        if i.sym == *"module" {
                            self.found_other = true;
                        }

                        if i.sym == *"exports" {
                            self.found_other = true;
                        }
                    }

                    _ => {}
                }
            }
            _ => {}
        }

        //
    }

    fn visit_module_decl(&mut self, decl: &ModuleDecl, _: &dyn Node) {
        match decl {
            ModuleDecl::Import(_)
            | ModuleDecl::ExportDecl(_)
            | ModuleDecl::ExportNamed(_)
            | ModuleDecl::ExportDefaultDecl(_)
            | ModuleDecl::ExportDefaultExpr(_)
            | ModuleDecl::ExportAll(_) => {
                self.forced_es6 = true;
            }

            ModuleDecl::TsImportEquals(_) => {}
            ModuleDecl::TsExportAssignment(_) => {}
            ModuleDecl::TsNamespaceExport(_) => {}
        }
    }
}
