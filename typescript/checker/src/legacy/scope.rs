use super::Analyzer;
use crate::{
    errors::Error,
    ty::{
        IndexSignature, Interface, PropertySignature, Tuple, Type, TypeElement, TypeLit, TypeRef,
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

#[derive(Debug, Clone)]
pub(super) struct VarInfo {
    pub kind: VarDeclKind,
    pub initialized: bool,
    pub ty: Option<Type<'static>>,
    /// Copied from parent scope. If this is true, it's not a variable
    /// declaration.
    pub copied: bool,
}

#[derive(Debug)]
pub(crate) struct Scope<'a> {
    /// Expanded type.
    ///
    /// e.g. `interface Foo { name: string; }` is saved as `{ 'Foo': { name:
    /// string; } }`
    pub(super) types: FxHashMap<JsWord, Type<'static>>,
    pub(super) this: Option<Type<'static>>,

    pub(super) declaring_fn: Option<JsWord>,
    /// `Some(name)` while declaring a class property.
    pub(super) declaring_prop: Option<JsWord>,

    kind: ScopeKind,
    /// Declared variables and parameters.
    ///
    /// TODO: Use vector (for performance)
    pub(super) vars: FxHashMap<JsWord, VarInfo>,
    pub(super) facts: CondFacts,
    parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub fn new(parent: &'a Scope<'a>, kind: ScopeKind, facts: CondFacts) -> Self {
        Scope {
            parent: Some(parent),
            types: Default::default(),
            this: None,
            kind,
            vars: Default::default(),
            facts,
            declaring_fn: None,
            declaring_prop: Default::default(),
        }
    }

    pub fn root() -> Self {
        Scope {
            parent: None,
            types: Default::default(),
            this: None,
            kind: ScopeKind::Fn,
            vars: Default::default(),
            facts: Default::default(),
            declaring_fn: None,
            declaring_prop: Default::default(),
        }
    }
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

    pub(super) fn depth(&self) -> usize {
        match self.parent {
            Some(ref p) => p.depth() + 1,
            None => 0,
        }
    }

    pub(super) fn this(&self) -> Option<&Type<'static>> {
        if let Some(ref this) = self.this {
            return Some(this);
        }

        match self.parent {
            Some(ref parent) => parent.this(),
            None => None,
        }
    }

    pub(super) fn get_var(&self, sym: &JsWord) -> Option<&VarInfo> {
        if let Some(ref v) = self.vars.get(sym) {
            return Some(v);
        }

        self.search_parent(sym)
    }

    pub(super) fn search_parent(&self, sym: &JsWord) -> Option<&VarInfo> {
        let mut parent = self.parent;

        while let Some(p) = parent {
            if let Some(var_info) = p.vars.get(sym) {
                return Some(var_info);
            }

            parent = p.parent;
        }

        None
    }

    /// Overrides a varaible. Used for removing lazily-typed stuffs.
    pub fn override_var(
        &mut self,
        kind: VarDeclKind,
        name: JsWord,
        ty: Type<'static>,
    ) -> Result<(), Error> {
        self.declare_var(ty.span(), kind, name, Some(ty), true, true)?;

        Ok(())
    }

    pub fn declare_complex_vars(
        &mut self,
        kind: VarDeclKind,
        pat: &Pat,
        ty: Type<'static>,
    ) -> Result<(), Error> {
        let span = pat.span();

        match *pat {
            Pat::Ident(ref i) => {
                println!("declare_complex_vars: declaring {}", i.sym);
                self.declare_var(
                    span,
                    kind,
                    i.sym.clone(),
                    Some(ty),
                    // initialized
                    true,
                    // let/const declarations does not allow multiple declarations with
                    // same name
                    kind == VarDeclKind::Var,
                )?;
                Ok(())
            }

            Pat::Array(ArrayPat { ref elems, .. }) => {
                // Handle tuple
                //
                //      const [a , setA] = useState();
                //

                // TODO: Normalize static
                match ty {
                    Type::Tuple(Tuple { types, .. }) => {
                        if types.len() < elems.len() {
                            return Err(Error::TooManyTupleElements { span });
                        }

                        for (elem, ty) in elems.into_iter().zip(types) {
                            match *elem {
                                Some(ref elem) => {
                                    self.declare_complex_vars(kind, elem, ty.into_owned())?;
                                }
                                None => {
                                    // Skip
                                }
                            }
                        }

                        return Ok(());
                    }

                    // [a, b] | [c, d] => [a | c, b | d]
                    Type::Union(Union { types, .. }) => {
                        let mut errors = vec![];
                        let mut buf: Vec<Vec<_>> = vec![];
                        for ty in types.iter() {
                            match *ty.normalize() {
                                Type::Tuple(Tuple {
                                    types: ref elem_types,
                                    ..
                                }) => {
                                    buf.push(
                                        elem_types
                                            .iter()
                                            .map(|v| v.to_static().into_cow())
                                            .collect(),
                                    );
                                }
                                _ => {
                                    errors.push(Error::NotTuple { span: ty.span() });
                                }
                            }
                        }
                        if !errors.is_empty() {
                            return Err(Error::UnionError { span, errors });
                        }

                        for (elem, types) in elems.into_iter().zip(
                            buf.into_iter()
                                .chain(repeat_with(|| vec![Type::undefined(span).owned()])),
                        ) {
                            match *elem {
                                Some(ref elem) => {
                                    let ty = Union { span, types }.into();
                                    self.declare_complex_vars(kind, elem, ty)?;
                                }
                                None => {}
                            }
                        }

                        return Ok(());
                    }

                    _ => unimplemented!("declare_complex_vars(pat={:?}\nty={:?}\n)", pat, ty),
                }
            }

            Pat::Object(ObjectPat { ref props, .. }) => {
                fn find<'a>(members: &[TypeElement<'a>], key: &PropName) -> Option<TypeRef<'a>> {
                    let mut index_el = None;
                    // First, we search for Property
                    for m in members {
                        match *m {
                            TypeElement::Property(PropertySignature { ref type_ann, .. }) => {
                                return match *type_ann {
                                    Some(ref ty) => Some(ty.clone()),
                                    None => Some(Type::any(key.span()).owned()),
                                }
                            }

                            TypeElement::Index(IndexSignature { ref type_ann, .. }) => {
                                index_el = Some(match *type_ann {
                                    Some(ref ty) => ty.clone(),
                                    None => Type::any(key.span()).owned(),
                                });
                            }
                            _ => {}
                        }
                    }

                    return index_el;
                }

                /// Handle TypeElements.
                ///
                /// Used for interfaces and type literals.
                macro_rules! handle_elems {
                    ($members:expr) => {{
                        for p in props.iter() {
                            match *p {
                                ObjectPatProp::KeyValue(KeyValuePatProp {
                                    ref key,
                                    ref value,
                                    ..
                                }) => {
                                    if let Some(ty) = find(&$members, key) {
                                        self.declare_complex_vars(kind, value, ty.to_static())?;
                                        return Ok(());
                                    }
                                }

                                _ => unimplemented!("handle_elems({:#?})", p),
                            }
                        }

                        return Err(Error::NoSuchProperty { span, prop: None });
                    }};
                }

                // TODO: Normalize static
                match ty {
                    Type::TypeLit(TypeLit { members, .. }) => {
                        handle_elems!(members);
                    }

                    // TODO: Handle extends
                    Type::Interface(Interface { body, .. }) => {
                        handle_elems!(body);
                    }

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {
                        for p in props.iter() {
                            match *p {
                                ObjectPatProp::KeyValue(ref kv) => {
                                    self.declare_complex_vars(
                                        kind,
                                        &kv.value,
                                        Type::any(kv.span()),
                                    )?;
                                }

                                _ => unimplemented!("handle_elems({:#?})", p),
                            }
                        }

                        return Ok(());
                    }

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsUnknownKeyword,
                        ..
                    }) => {
                        // TODO: Somehow get precise logic of determining span.
                        //
                        // let { ...a } = x;
                        //          ^
                        //

                        // WTF...
                        for p in props.iter().rev() {
                            let span = match p {
                                ObjectPatProp::Rest(RestPat { ref arg, .. }) => arg.span(),
                                _ => p.span(),
                            };
                            return Err(Error::Unknown { span });
                        }

                        return Err(Error::Unknown { span });
                    }

                    _ => unimplemented!("declare_complex_vars({:#?}, {:#?})", pat, ty),
                }
            }

            _ => unimplemented!("declare_complex_vars({:#?}, {:#?})", pat, ty),
        }
    }

    pub fn declare_var(
        &mut self,
        span: Span,
        kind: VarDeclKind,
        name: JsWord,
        ty: Option<Type<'static>>,
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
                    TsType::TsTypeRef(..) => panic!("Var's kind should not be TypeRef"),
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
                        Type::union(once(ty).chain(once(var_ty)))
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

    /// This method does cannot handle imported types.
    pub(super) fn find_type(&self, name: &JsWord) -> Option<&Type<'static>> {
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

    /// # Interface
    ///
    /// Registers an interface, and merges it with previous interface
    /// declaration if required.
    pub fn register_type(&mut self, name: JsWord, ty: Type<'static>) {
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
                e.insert(ty.into_static());
            }
        }
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
                    match self.expand_type(i.span, ty.owned()) {
                        Ok(ty) => Some(ty.to_static()),
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

    pub(super) fn find_var_type<'a>(&'a self, name: &JsWord) -> Option<&'a Type<'static>> {
        // println!("({}) find_var_type({})", self.scope.depth(), name);
        let mut scope = Some(&self.scope);
        while let Some(s) = scope {
            if let Some(ref v) = s.facts.vars.get(&Name::from(name)) {
                println!(
                    "({}) find_var_type({}): Handled from facts",
                    self.scope.depth(),
                    name
                );
                return Some(v);
            }

            scope = s.parent;
        }

        if let Some(var) = self.find_var(name) {
            println!(
                "({}) find_var_type({}): Handled from scope.find_var",
                self.scope.depth(),
                name
            );

            let name = Name::from(name);

            let ty = match var.ty {
                Some(ref ty) => ty,
                _ => return None,
            };

            if let Some(ref excludes) = self.scope.facts.excludes.get(&name) {
                let mut ty = Cow::Borrowed(ty);
                match *ty.normalize_mut() {
                    Type::Union(Union { ref mut types, .. }) => {
                        for ty in types {
                            let span = ty.span();
                            for excluded_ty in excludes.iter() {
                                if ty.eq_ignore_name_and_span(excluded_ty) {
                                    *ty = Type::never(span).owned()
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            return Some(ty);
        }

        None
    }

    pub(super) fn find_type<'a>(&'a self, name: &JsWord) -> Option<&'a Type<'static>> {
        #[allow(dead_code)]
        static ANY: Type = Type::Keyword(TsKeywordType {
            span: DUMMY_SP,
            kind: TsKeywordTypeKind::TsAnyKeyword,
        });

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
