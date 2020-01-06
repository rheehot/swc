use crate::{
    analyzer::Analyzer,
    errors::Error,
    loader::Load,
    ty::{self, Class, Module, Static},
    validator::{Validate, ValidateWith},
    Exports, ImportInfo,
};
use chashmap::CHashMap;
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use std::{collections::hash_map::Entry, path::PathBuf, sync::Arc};
use swc_atoms::JsWord;
use swc_common::{Span, VisitWith, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ts_builtin_types::load;
pub use swc_ts_builtin_types::Lib;

type Type = ty::Type;

#[derive(Debug, Default)]
struct Merged {
    vars: FxHashMap<JsWord, Type>,
    types: FxHashMap<JsWord, Type>,
}

fn merge(ls: &[Lib]) -> &'static Merged {
    lazy_static! {
        static ref CACHE: CHashMap<Vec<Lib>, &'static Merged> = Default::default();
    }

    let mut libs = ls.to_vec();
    if libs.is_empty() {
        libs.push(Lib::Es5);
    }
    let libs = libs;
    if let Some(cached) = CACHE.get(&libs) {
        return &*cached;
    }

    // We hold write lock (thus block readers) while merging.
    CACHE.alter(libs, |v| {
        if let Some(v) = v {
            return Some(v);
        }

        let mut merged = box Merged::default();
        let mut analyzer = Analyzer::for_builtin();

        for module in load(ls) {
            match *module.body {
                TsNamespaceBody::TsModuleBlock(TsModuleBlock { ref body, .. }) => {
                    for item in body {
                        match item {
                            ModuleItem::ModuleDecl(ref md) => unreachable!("ModuleDecl: {:#?}", md),
                            ModuleItem::Stmt(ref stmt) => match *stmt {
                                Stmt::Decl(Decl::Var(VarDecl { ref decls, .. })) => {
                                    assert_eq!(decls.len(), 1);
                                    let decl = decls.iter().next().unwrap();
                                    let name = match decl.name {
                                        Pat::Ident(ref i) => i,
                                        _ => unreachable!(),
                                    };
                                    merged.vars.insert(
                                        name.sym.clone(),
                                        name.type_ann
                                            .validate_with(&mut analyzer)
                                            .map(|res| {
                                                res.expect(
                                                    "builtin: failed to parse type of a variable",
                                                )
                                            })
                                            .expect("builtin: all variables should have a type"),
                                    );
                                }

                                Stmt::Decl(Decl::Fn(FnDecl {
                                    ref ident,
                                    ref function,
                                    ..
                                })) => {
                                    merged.types.insert(
                                        ident.sym.clone(),
                                        ty::Function {
                                            span: DUMMY_SP,
                                            params: function
                                                .params
                                                .validate_with(&mut analyzer)
                                                .expect(
                                                    "builtin: failed to parse parameters of a \
                                                     function",
                                                ),
                                            type_params: function
                                                .type_params
                                                .validate_with(&mut analyzer)
                                                .map(|res| {
                                                    res.expect(
                                                        "builtin: failed to parse type parameters \
                                                         of a function",
                                                    )
                                                }),
                                            ret_ty: box function
                                                .return_type
                                                .validate_with(&mut analyzer)
                                                .map(|res| {
                                                    res.expect(
                                                        "builtin: failed to parse return type of \
                                                         a function",
                                                    )
                                                })
                                                .unwrap_or_else(|| Type::any(ident.span)),
                                        }
                                        .into(),
                                    );
                                }

                                Stmt::Decl(Decl::Class(ref c)) => {
                                    debug_assert_eq!(merged.types.get(&c.ident.sym), None);

                                    // builtin libraries does not contain a class which extends
                                    // other class.
                                    debug_assert_eq!(c.class.super_class, None);
                                    debug_assert_eq!(c.class.implements, vec![]);
                                    let ty = Type::Class(Class {
                                        span: c.class.span,
                                        name: Some(c.ident.sym.clone()),
                                        is_abstract: c.class.is_abstract,
                                        body: analyzer
                                            .validate(&c.class.body)
                                            .expect("builtin: failed to validate class body")
                                            .into_iter()
                                            .filter_map(|v| v)
                                            .collect(),
                                        super_class: None,
                                        // implements: vec![],
                                        type_params: c
                                            .class
                                            .type_params
                                            .validate_with(&mut analyzer)
                                            .map(|opt| {
                                                opt.expect(
                                                    "builtin: failed to parse type parmas of a \
                                                     class",
                                                )
                                            }),
                                    });

                                    merged.types.insert(c.ident.sym.clone(), ty);
                                }

                                Stmt::Decl(Decl::TsModule(ref m)) => {
                                    let id = match m.id {
                                        TsModuleName::Ident(ref i) => i.sym.clone(),
                                        _ => unreachable!(),
                                    };

                                    let mut analyzer = Analyzer::for_builtin();

                                    m.body.visit_with(&mut analyzer);

                                    match merged.types.entry(id) {
                                        Entry::Occupied(mut e) => match e.get_mut() {
                                            ty::Type::Module(module) => {
                                                //
                                                module.exports.extend(analyzer.info.exports)
                                            }

                                            ref e => unimplemented!("Merging module with {:?}", e),
                                        },
                                        Entry::Vacant(e) => {
                                            e.insert(
                                                Module {
                                                    span: DUMMY_SP,
                                                    exports: analyzer.info.exports,
                                                }
                                                .into(),
                                            );
                                        }
                                    }
                                }

                                Stmt::Decl(Decl::TsTypeAlias(ref a)) => {
                                    debug_assert_eq!(merged.types.get(&a.id.sym), None);

                                    let ty = a
                                        .validate_with(&mut analyzer)
                                        .map(Type::from)
                                        .expect("builtin: failed to process type alias");

                                    merged.types.insert(a.id.sym.clone(), ty);
                                }

                                // Merge interface
                                Stmt::Decl(Decl::TsInterface(ref i)) => {
                                    match merged.types.entry(i.id.sym.clone()) {
                                        Entry::Occupied(mut e) => match *e.get_mut() {
                                            ty::Type::Interface(ref mut v) => {
                                                v.body.extend(
                                                    analyzer.validate(&i.body.body).expect(
                                                        "builtin: failed to parse interface body",
                                                    ),
                                                );
                                            }
                                            _ => unreachable!(
                                                "cannot merge interface with other type"
                                            ),
                                        },
                                        Entry::Vacant(e) => {
                                            e.insert(
                                                i.validate_with(&mut analyzer)
                                                    .expect("builtin: failed to parse interface")
                                                    .into(),
                                            );
                                        }
                                    }
                                }

                                _ => panic!("{:#?}", item),
                            },
                        }
                    }
                }
                _ => unreachable!(),
            }
        }

        assert_eq!(analyzer.info.errors, vec![]);

        Some(Box::leak(merged))
    });

    return &*CACHE.get(ls).unwrap();
}

pub fn get_var(libs: &[Lib], span: Span, name: &JsWord) -> Result<Type, Error> {
    let lib = merge(libs);

    if let Some(v) = lib.vars.get(&name) {
        return Ok(ty::Type::Static(Static { span, ty: v }));
    }

    Err(Error::NoSuchVar {
        span,
        name: name.clone(),
    })
}

pub fn get_type(libs: &[Lib], span: Span, name: &JsWord) -> Result<Type, Error> {
    let lib = merge(libs);

    if let Some(ty) = lib.types.get(name) {
        return Ok(ty::Type::Static(Static { span, ty }));
    }

    Err(Error::NoSuchType {
        span,
        name: name.clone(),
    })
}

struct Noop;

impl Load for Noop {
    fn load(
        &self,
        base: Arc<PathBuf>,
        import: &ImportInfo,
    ) -> Result<Exports<FxHashMap<JsWord, Type>>, Error> {
        unimplemented!()
    }
}
