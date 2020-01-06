use super::Analyzer;
use crate::{
    errors::Error,
    ty::{
        IndexSignature, Interface, PropertySignature, Tuple, Type, TypeElement, TypeLit,
        TypeRefExt, Union,
    },
    util::{EqIgnoreNameAndSpan, IntoCow},
};
use fxhash::FxHashMap;
use std::{
    borrow::Cow,
    collections::hash_map::Entry,
    iter::{once, repeat_with},
    sync::Arc,
};
use swc_atoms::JsWord;
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

#[derive(Debug)]
pub(crate) struct Scope<'a> {
    /// Expanded type.
    ///
    /// e.g. `interface Foo { name: string; }` is saved as `{ 'Foo': { name:
    /// string; } }`
    pub(super) types: FxHashMap<JsWord, Type>,
    pub(super) this: Option<Type>,

    pub(super) declaring_fn: Option<JsWord>,
    /// `Some(name)` while declaring a class property.
    pub(super) declaring_prop: Option<JsWord>,

    kind: ScopeKind,
    /// Declared variables and parameters.
    ///
    /// TODO: Use vector (for performance)
    pub(super) vars: FxHashMap<JsWord, VarInfo>,
    parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub(super) fn find_declaring_fn(&self, name: &JsWord) -> bool {
        if let Some(ref d_fn) = self.declaring_fn {
            if *d_fn == *name {
                return true;
            }
        }

        match self.parent {
            Some(ref parent) => parent.find_declaring_fn(name),
            None => false,
        }
    }

    /// Overrides a varaible. Used for removing lazily-typed stuffs.
    pub fn override_var(&mut self, kind: VarDeclKind, name: JsWord, ty: Type) -> Result<(), Error> {
        self.declare_var(ty.span(), kind, name, Some(ty), true, true)?;

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    pub fn declare_vars(&mut self, kind: VarDeclKind, pat: &Pat) -> Result<(), Error> {
        self.declare_vars_inner(kind, pat, false)
    }

    /// Updates variable list.
    ///
    /// This method should be called for function parameters including error
    /// variable from a catch clause.
    pub fn declare_vars_inner(
        &mut self,
        kind: VarDeclKind,
        pat: &Pat,
        export: bool,
    ) -> Result<(), Error> {
        match *pat {
            Pat::Ident(ref i) => {
                let ty = i
                    .type_ann
                    .as_ref()
                    .map(|t| &*t.type_ann)
                    .cloned()
                    .map(Type::from);
                let ty = if let Some(ty) = ty {
                    match self.expand_type(i.span, ty) {
                        Ok(ty) => Some(ty),
                        Err(err) => {
                            self.info.errors.push(err);
                            return Ok(());
                        }
                    }
                } else {
                    None
                };

                let name = i.sym.clone();
                self.scope.declare_var(
                    ty.span(),
                    kind,
                    name.clone(),
                    ty.clone(),
                    // initialized
                    true,
                    // allow_multiple
                    kind == VarDeclKind::Var,
                )?;
                if export {
                    if let Some(..) = self
                        .info
                        .exports
                        .insert(name, Arc::new(ty.unwrap_or(Type::any(i.span))))
                    {
                        unimplemented!("multiple exported variables with same name")
                    }
                }
                return Ok(());
            }
            Pat::Assign(ref p) => {
                let ty = self.type_of(&p.right)?;
                println!(
                    "({}) declare_vars({:?}), ty = {:?}",
                    self.scope.depth(),
                    p.left,
                    ty
                );
                self.declare_vars_inner(kind, &p.left, export)?;

                return Ok(());
            }

            Pat::Array(ArrayPat { ref elems, .. }) => {
                // TODO: Handle type annotation

                for elem in elems {
                    match *elem {
                        Some(ref elem) => {
                            self.declare_vars_inner(kind, elem, export)?;
                        }
                        // Skip
                        None => {}
                    }
                }

                return Ok(());
            }

            Pat::Object(ObjectPat { ref props, .. }) => {
                for prop in props {
                    match *prop {
                        ObjectPatProp::KeyValue(KeyValuePatProp { .. }) => {
                            unimplemented!("ket value pattern in object pattern")
                        }
                        ObjectPatProp::Assign(AssignPatProp { .. }) => {
                            unimplemented!("assign pattern in object pattern")
                        }
                        ObjectPatProp::Rest(RestPat { .. }) => {
                            unimplemented!("rest pattern in object pattern")
                        }
                    }
                }

                return Ok(());
            }

            Pat::Rest(RestPat {
                ref arg,
                type_ann: ref ty,
                ..
            }) => {
                let ty = ty.clone();
                let mut arg = arg.clone();
                match *arg {
                    Pat::Ident(ref mut v) => v.type_ann = ty,
                    Pat::Array(ref mut v) => v.type_ann = ty,
                    Pat::Object(ref mut v) => v.type_ann = ty,
                    Pat::Assign(ref mut v) => v.type_ann = ty,

                    // TODO: check if Pat(Expr) is really unreachable
                    // `foo.bar = 1`
                    Pat::Rest(_) | Pat::Expr(_) | Pat::Invalid(..) => unreachable!(),
                }
                return self.declare_vars_inner(kind, &arg, export);
            }

            _ => unimplemented!("declare_vars for patterns other than ident: {:#?}", pat),
        }
    }
}
