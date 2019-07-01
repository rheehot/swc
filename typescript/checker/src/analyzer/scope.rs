use super::{control_flow::CondFacts, Analyzer, Name};
use crate::{
    errors::Error,
    ty::{
        IndexSignature, Interface, PropertySignature, Tuple, Type, TypeElement, TypeLit, TypeRef,
        TypeRefExt, Union,
    },
    util::{EqIgnoreNameAndSpan, IntoCow},
};
use fxhash::FxHashMap;
use std::{collections::hash_map::Entry, iter::repeat_with};
use swc_atoms::JsWord;
use swc_common::{Spanned, DUMMY_SP};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ScopeKind {
    Block,
    Fn,
    /// If statement, conditional expression, switch case
    Flow,
}

#[derive(Debug)]
pub(super) struct Scope<'a> {
    /// Expanded type.
    ///
    /// e.g. `interface Foo { name: string; }` is saved as `{ 'Foo': { name:
    /// string; } }`
    pub(super) types: FxHashMap<JsWord, Type<'static>>,
    pub(super) this: Option<Type<'static>>,

    pub(super) declaring_fn: Option<JsWord>,

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
        self.declare_var(kind, name, Some(ty), true, true)?;

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
                self.declare_var(
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
                // const [a , setA] = useState();
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

                    _ => unimplemented!("declare_complex_vars({:#?}, {:#?})", pat, ty),
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

                        return Err(Error::NoSuchProperty { span });
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
                    }) => return Err(Error::Unknown { span }),

                    _ => unimplemented!("declare_complex_vars({:#?}, {:#?})", pat, ty),
                }
            }

            _ => unimplemented!("declare_complex_vars({:#?}, {:#?})", pat, ty),
        }
    }

    pub fn declare_var(
        &mut self,
        kind: VarDeclKind,
        name: JsWord,
        ty: Option<Type<'static>>,
        initialized: bool,
        allow_multiple: bool,
    ) -> Result<(), Error> {
        let span = ty.span();

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
                let (k, mut v) = e.remove_entry();

                v.ty = if let Some(ty) = ty {
                    Some(if let Some(var_ty) = v.ty {
                        merge_type(ty, var_ty)
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

                let info = VarInfo { ..v };
                self.vars.insert(k, info);
            }
            Entry::Vacant(e) => {
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
        if let Some(ty) = self.types.get(name) {
            println!("({}) find_type({}): Found", self.depth(), name);
            return Some(&ty);
        }

        if let Some(ty) = self.facts.types.get(name) {
            println!("({}) find_type({}): Found (cond facts)", self.depth(), name);
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
    /// Updates variable list.
    ///
    /// This method should be called for function parameters including error
    /// variable from a catch clause.
    pub fn declare_vars(&mut self, kind: VarDeclKind, pat: &Pat) -> Result<(), Error> {
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
                    kind,
                    name,
                    ty,
                    // initialized
                    true,
                    // allow_multiple
                    kind == VarDeclKind::Var,
                )?;
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
                self.declare_vars(kind, &p.left)?;

                return Ok(());
            }

            Pat::Array(ArrayPat { ref elems, .. }) => {
                // TODO: Handle type annotation

                for elem in elems {
                    match *elem {
                        Some(ref elem) => {
                            self.declare_vars(kind, elem)?;
                        }
                        // Skip
                        None => {}
                    }
                }

                return Ok(());
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

            match var.ty {
                Some(ref ty) => return Some(ty),
                _ => {}
            }
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

fn merge_type<'a>(l: Type<'a>, r: Type<'a>) -> Type<'a> {
    let span = l.span();
    if l.eq_ignore_name_and_span(&r) {
        return l;
    }
    let mut buf = Vec::with_capacity(2);

    macro_rules! handle {
        ($ty:expr) => {{
            match $ty {
                Type::Union(Union { types, .. }) => {
                    buf.extend(types);
                }
                ty => buf.push(ty.into_cow()),
            }
        }};
    }

    handle!(l);
    handle!(r);

    Type::Union(Union { span, types: buf })
}
