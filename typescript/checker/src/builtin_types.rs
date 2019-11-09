use crate::{
    analyzer::{export::pat_to_ts_fn_param, Analyzer, ImportInfo},
    errors::Error,
    loader::Load,
    ty::{self, Class, ClassMember, Module, Static},
};
use chashmap::CHashMap;
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use std::{
    collections::hash_map::Entry,
    path::{Path, PathBuf},
    sync::Arc,
};
use swc_atoms::JsWord;
use swc_common::{Span, VisitWith, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ts_builtin_types::load;
pub use swc_ts_builtin_types::Lib;

type Type = ty::Type<'static>;

#[derive(Debug, Default)]
struct Merged {
    vars: FxHashMap<JsWord, Type>,
    types: FxHashMap<JsWord, Type>,
}

fn merge(ls: &[Lib]) -> &'static Merged {
    lazy_static! {
        static ref CACHE: CHashMap<Vec<Lib>, &'static Merged> = Default::default();
    }

    let libs = ls.to_vec();
    if let Some(cached) = CACHE.get(&libs) {
        return &*cached;
    }

    // We hold write lock (thus block readers) while merging.
    CACHE.alter(libs, |v| {
        if let Some(v) = v {
            return Some(v);
        }

        let mut merged = box Merged::default();

        for module in load(ls) {
            match *module.body {
                TsNamespaceBody::TsModuleBlock(TsModuleBlock { ref body, .. }) => {
                    for item in body {
                        match item {
                            ModuleItem::ModuleDecl(ref md) => unreachable!("ModuleDecl: {:#?}", md),
                            ModuleItem::Stmt(ref stmt) => match *stmt {
                                Stmt::Decl(Decl::Var(VarDecl { ref decls, .. })) => {
                                    assert!(decls.len() == 1);
                                    let decl = decls.iter().next().unwrap();
                                    let name = match decl.name {
                                        Pat::Ident(ref i) => i,
                                        _ => unreachable!(),
                                    };
                                    merged.vars.insert(
                                        name.sym.clone(),
                                        name.type_ann.clone().unwrap().into(),
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
                                                .iter()
                                                .cloned()
                                                .map(pat_to_ts_fn_param)
                                                .collect(),
                                            type_params: function
                                                .type_params
                                                .clone()
                                                .map(From::from),
                                            ret_ty: box function
                                                .return_type
                                                .clone()
                                                .map(|v| v.type_ann.into())
                                                .unwrap_or_else(|| Type::any(DUMMY_SP))
                                                .owned(),
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
                                        body: c
                                            .class
                                            .body
                                            .clone()
                                            .into_iter()
                                            .map(|v| {
                                                //
                                                match v {
                                                    swc_ecma_ast::ClassMember::Constructor(v) => {
                                                        ClassMember::Constructor(v.into())
                                                    }
                                                    swc_ecma_ast::ClassMember::Method(v) => {
                                                        ClassMember::Method(v.into())
                                                    }
                                                    swc_ecma_ast::ClassMember::PrivateMethod(_) => {
                                                        unreachable!()
                                                    }
                                                    swc_ecma_ast::ClassMember::ClassProp(v) => {
                                                        ClassMember::ClassProp(v)
                                                    }
                                                    swc_ecma_ast::ClassMember::PrivateProp(_) => {
                                                        unreachable!()
                                                    }
                                                    swc_ecma_ast::ClassMember::TsIndexSignature(
                                                        v,
                                                    ) => ClassMember::TsIndexSignature(v),
                                                }
                                            })
                                            .collect(),
                                        super_class: None,
                                        // implements: vec![],
                                        type_params: c.class.type_params.clone().map(From::from),
                                    });

                                    merged.types.insert(c.ident.sym.clone(), ty);
                                }

                                Stmt::Decl(Decl::TsModule(ref m)) => {
                                    let id = match m.id {
                                        TsModuleName::Ident(ref i) => i.sym.clone(),
                                        _ => unreachable!(),
                                    };

                                    let mut analyzer = Analyzer::for_builtin(&ls, &Noop);

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

                                    merged.types.insert(a.id.sym.clone(), a.clone().into());
                                }

                                // Merge interface
                                Stmt::Decl(Decl::TsInterface(ref i)) => {
                                    match merged.types.entry(i.id.sym.clone()) {
                                        Entry::Occupied(mut e) => match *e.get_mut() {
                                            ty::Type::Interface(ref mut v) => {
                                                v.body.extend(
                                                    i.body.body.clone().into_iter().map(From::from),
                                                );
                                            }
                                            _ => unreachable!(
                                                "cannot merge interface with other type"
                                            ),
                                        },
                                        Entry::Vacant(e) => {
                                            e.insert(i.clone().into());
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

        Some(Box::leak(merged))
    });

    return &*CACHE.get(ls).unwrap();
}

pub fn get_var(libs: &[Lib], span: Span, name: &JsWord) -> Result<Type, Error> {
    let lib = merge(libs);

    if let Some(v) = lib.vars.get(&name) {
        return Ok(ty::Type::Static(Static { span, ty: v }));
    }

    Err(Error::NoSuchType {
        span,
        name: name.clone(),
    })
}

pub fn get_type(libs: &[Lib], span: Span, name: &JsWord) -> Result<Type, Error> {
    let lib = merge(libs);

    if let Some(ty) = lib.types.get(name) {
        return Ok(ty::Type::Static(Static { span, ty }));
    }

    Err(Error::NoSuchVar {
        span,
        name: name.clone(),
    })
}

struct Noop;

impl Load for Noop {
    fn load(&self, _: Arc<PathBuf>, _: &ImportInfo) -> Result<FxHashMap<JsWord, Arc<Type>>, Error> {
        unimplemented!()
    }
}
