use super::{control_flow::CondFacts, Analyzer};
use crate::{errors::Error, ty::Type};
use fxhash::FxHashMap;
use smallvec::SmallVec;
use std::{collections::hash_map::Entry, sync::Arc};
use swc_atoms::JsWord;
use swc_common::{Span, DUMMY_SP};
use swc_ecma_ast::*;

#[derive(Debug)]
pub(crate) struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
    pub declaring: SmallVec<[JsWord; 8]>,

    pub(super) vars: FxHashMap<JsWord, VarInfo>,
    pub(super) types: FxHashMap<JsWord, Type>,
    pub(super) facts: CondFacts,

    pub(super) this: Option<JsWord>,
}

impl Scope<'_> {
    pub fn remove_declaring<I>(&mut self, names: impl IntoIterator<IntoIter = I, Item = JsWord>)
    where
        I: Iterator<Item = JsWord> + DoubleEndedIterator,
    {
        for n in names.into_iter().rev() {
            let idx = self
                .declaring
                .iter()
                .rposition(|name| n == *name)
                .expect("failed to find inserted name");
            self.declaring.remove(idx);
        }
    }

    /// # Interface
    ///
    /// Registers an interface, and merges it with previous interface
    /// declaration if required.
    pub fn register_type(&mut self, name: JsWord, ty: Type) {
        let depth = self.depth();

        if cfg!(debug_assertions) {
            match ty.normalize() {
                Type::Alias(ref alias) => {
                    //
                    if alias.type_params.is_none() {
                        match **alias.ty {
                            Type::Simple(ref s_ty) => match **s_ty {
                                TsType::TsTypeRef(..) => panic!(
                                    "Type alias without type parameters should be expanded before \
                                     .register_type()"
                                ),
                                _ => {}
                            },
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }

        match self.types.entry(name) {
            Entry::Occupied(mut e) => {
                println!("({}) register_type({}): duplicate", depth, e.key());

                // TODO: Match -> .map
                match (e.get_mut(), ty) {
                    (&mut Type::Interface(ref mut orig), Type::Interface(ref mut i)) => {
                        // TODO: Check fields' type
                        // TODO: Sort function members like
                        // https://www.typescriptlang.org/docs/handbook/declaration-merging.html#merging-interfaces
                        orig.body.append(&mut i.body);
                    }
                    ref ty => unreachable!("{:?} cannot be merged with {:?}", ty.0, ty.1),
                }
            }
            Entry::Vacant(e) => {
                println!("({}) register_type({})", depth, e.key());
                e.insert(ty);
            }
        }
    }

    pub fn this(&self) -> Option<&Type> {
        if let Some(ref this) = self.this {
            return Some(this);
        }

        match self.parent {
            Some(ref parent) => parent.this(),
            None => None,
        }
    }

    pub fn get_var(&self, sym: &JsWord) -> Option<&VarInfo> {
        if let Some(ref v) = self.vars.get(sym) {
            return Some(v);
        }

        self.search_parent(sym)
    }

    pub fn search_parent(&self, sym: &JsWord) -> Option<&VarInfo> {
        let mut parent = self.parent;

        while let Some(p) = parent {
            if let Some(var_info) = p.vars.get(sym) {
                return Some(var_info);
            }

            parent = p.parent;
        }

        None
    }

    pub fn declare_var(
        &mut self,
        span: Span,
        kind: VarDeclKind,
        name: JsWord,
        ty: Option<Type>,
        initialized: bool,
        allow_multiple: bool,
    ) -> Result<(), Error> {
        println!(
            "({}) declare_var({}, initialized = {:?})",
            self.depth(),
            name,
            initialized,
        );

        if cfg!(debug_assertions) {
            match ty {
                Some(Type::Simple(ref t)) => match **t {
                    TsType::TsTypeRef(..) => panic!("Var's kind should not be Type"),
                    _ => {}
                },
                _ => {}
            }
        }

        match self.vars.entry(name) {
            Entry::Occupied(e) => {
                if !allow_multiple {
                    return Err(Error::DuplicateName { span });
                }
                println!("\tdeclare_var: found entry ({:?})", e.get());
                let (k, mut v) = e.remove_entry();

                macro_rules! restore {
                    () => {{
                        self.vars.insert(k, v);
                    }};
                }

                v.ty = if let Some(ty) = ty {
                    let ty = ty.generalize_lit().into_owned();

                    Some(if let Some(var_ty) = v.ty {
                        println!("\tdeclare_var: ty = {:?}", ty);
                        let var_ty = var_ty.generalize_lit().into_owned();

                        // if k.as_ref() == "co1" {
                        //     v.ty = Some(var_ty);
                        //     restore!();
                        //     return Err(Error::RedclaredVarWithDifferentType { span });
                        // }

                        match ty {
                            Type::Function(..) => {}
                            _ => {
                                let generalized_var_ty = var_ty.clone().generalize_lit();
                                if !ty.eq_ignore_name_and_span(&generalized_var_ty) {
                                    v.ty = Some(var_ty);
                                    restore!();
                                    return Err(Error::RedclaredVarWithDifferentType { span });
                                }
                            }
                        }
                        Type::union(vec![ty, var_ty])
                    } else {
                        ty
                    })
                } else {
                    if let Some(var_ty) = v.ty {
                        Some(var_ty)
                    } else {
                        None
                    }
                };

                self.vars.insert(k, v);
            }
            Entry::Vacant(e) => {
                println!("\tdeclare_var: no entry");

                let info = VarInfo {
                    kind,
                    ty,
                    initialized,
                    copied: false,
                };
                e.insert(info);
            }
        }

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
    fn declare_vars_inner(
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
                        .vars
                        .insert(name, Arc::new(ty.unwrap_or(Type::any(i.span))))
                    {
                        unimplemented!("multiple exported variables with same name")
                    }
                }
                return Ok(());
            }
            Pat::Assign(ref p) => {
                let ty = self.validate(&p.right)?;
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
                let arg = arg.clone();
                return self.declare_vars_inner(kind, &arg, export);
            }

            _ => unimplemented!("declare_vars for patterns other than ident: {:#?}", pat),
        }
    }

    #[inline(never)]
    pub(super) fn find_var(&self, name: &JsWord) -> Option<&VarInfo> {
        static ERR_VAR: VarInfo = VarInfo {
            ty: Some(Type::any(DUMMY_SP)),
            kind: VarDeclKind::Const,
            initialized: true,
            copied: false,
        };

        if self.errored_imports.get(name).is_some() {
            return Some(&ERR_VAR);
        }

        let mut scope = Some(&self.scope);

        while let Some(s) = scope {
            if let Some(var) = s.vars.get(name) {
                return Some(var);
            }

            scope = s.parent;
        }

        None
    }

    #[inline(never)]
    pub(super) fn find_type<'a>(&'a self, name: &JsWord) -> Option<&'a Type> {
        #[allow(dead_code)]
        static ANY: Type = Type::any(DUMMY_SP);

        if self.errored_imports.get(name).is_some() {
            return Some(&ANY);
        }

        if let Some(ty) = self.resolved_imports.get(name) {
            return Some(ty);
        }

        if let Some(ty) = self.scope.find_type(name) {
            return Some(ty);
        }

        None
    }
}

#[derive(Debug, Clone)]
pub(crate) struct VarInfo {
    pub kind: VarDeclKind,
    pub initialized: bool,
    pub ty: Option<Type>,
    /// Copied from parent scope. If this is true, it's not a variable
    /// declaration.
    pub copied: bool,
}

impl<'a> Scope<'a> {
    pub const fn kind(&self) -> ScopeKind {
        self.kind
    }

    pub fn new(parent: &'a Scope<'a>, kind: ScopeKind, facts: CondFacts) -> Self {
        Self::new_inner(Some(parent), kind, facts)
    }

    pub fn root() -> Self {
        Self::new_inner(None, ScopeKind::Fn, Default::default())
    }

    fn new_inner(parent: Option<&'a Scope<'a>>, kind: ScopeKind, facts: CondFacts) -> Self {
        Scope {
            parent,

            kind,
            declaring: Default::default(),
            vars: Default::default(),
            types: Default::default(),
            facts,
            this: None,
        }
    }

    pub(super) fn depth(&self) -> usize {
        match self.parent {
            Some(ref p) => p.depth() + 1,
            None => 0,
        }
    }

    /// This method does **not** handle imported types.
    pub(super) fn find_type(&self, name: &JsWord) -> Option<&Type> {
        if let Some(ty) = self.facts.types.get(name) {
            println!("({}) find_type({}): Found (cond facts)", self.depth(), name);
            return Some(&ty);
        }

        if let Some(ty) = self.types.get(name) {
            println!("({}) find_type({}): Found", self.depth(), name);

            return Some(&ty);
        }

        match self.parent {
            Some(ref parent) => parent.find_type(name),
            None => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ScopeKind {
    Block,
    Fn,
    ArrowFn,
    /// If statement, conditional expression, switch case
    Flow,
}
