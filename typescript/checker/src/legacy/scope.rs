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

    pub fn declare_complex_vars(
        &mut self,
        kind: VarDeclKind,
        pat: &Pat,
        ty: Type,
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
                                    buf.push(elem_types.iter().map(|v| v.into_cow()).collect());
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
                fn find<'a>(members: &[TypeElement<'a>], key: &PropName) -> Option<Type<'a>> {
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

    pub(super) fn find_var_type<'a>(&'a self, name: &JsWord) -> Option<&'a Type> {
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
}
