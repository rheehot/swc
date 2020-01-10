use super::{control_flow::CondFacts, Analyzer};
use crate::{
    builtin_types,
    errors::Error,
    name::Name,
    ty::{
        self, Alias, Array, EnumVariant, IndexSignature, Interface, Intersection,
        PropertySignature, QueryExpr, QueryType, Ref, Tuple, Type, TypeElement, TypeLit, Union,
    },
    util::{EqIgnoreNameAndSpan, EqIgnoreSpan},
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use fxhash::FxHashMap;
use smallvec::SmallVec;
use std::{borrow::Cow, collections::hash_map::Entry, iter::repeat};
use swc_atoms::{js_word, JsWord};
use swc_common::{Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;

macro_rules! no_ref {
    ($t:expr) => {{
        match $t {
            Some(Type::Ref(..)) => panic!("cannot store a variable with type `Ref`"),
            _ => {}
        }
    }};
}

#[derive(Debug)]
pub(crate) struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
    pub declaring: SmallVec<[JsWord; 8]>,

    vars: FxHashMap<JsWord, VarInfo>,
    pub(super) types: FxHashMap<JsWord, Type>,
    pub(super) facts: CondFacts,

    pub(super) declaring_fn: Option<JsWord>,
    /// [Some] while declaring a class property.
    pub(super) declaring_prop: Option<JsWord>,

    pub(super) this: Option<JsWord>,
    pub(super) this_class_name: Option<JsWord>,
}

impl Scope<'_> {
    pub fn remove_parent(self) -> Scope<'static> {
        Scope {
            parent: None,
            kind: self.kind,
            declaring: self.declaring,
            vars: self.vars,
            types: self.types,
            facts: self.facts,
            declaring_fn: self.declaring_fn,
            declaring_prop: self.declaring_prop,
            this: self.this,
            this_class_name: self.this_class_name,
        }
    }

    pub fn copy_hoisted_vars_from(&mut self, from: &mut Scope) {
        match from.kind {
            // We don't copy variable information from nested function.
            ScopeKind::Fn | ScopeKind::ArrowFn => return,
            _ => {}
        }

        for (name, var) in from.vars.drain() {
            if var.kind == VarDeclKind::Var {
                self.vars.insert(name, var);
            }
        }
    }

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

    pub fn insert_var(&mut self, name: JsWord, v: VarInfo) {
        no_ref!(v.ty);

        self.vars.insert(name, v);
    }

    /// This method does **not** search for parent scope.
    pub fn get_var_mut(&mut self, name: &JsWord) -> Option<&mut VarInfo> {
        self.vars.get_mut(name)
    }

    /// # Interface
    ///
    /// Registers an interface, and merges it with previous interface
    /// declaration if required.
    fn register_type(&mut self, name: JsWord, ty: Type) {
        let depth = self.depth();

        if cfg!(debug_assertions) {
            match ty.normalize() {
                Type::Alias(ref alias) => {
                    //
                    if alias.type_params.is_none() {
                        match *alias.ty {
                            Type::Ref(ref r) => panic!(
                                "Type alias without type parameters should be expanded before \
                                 .register_type()\nRef: {:?}",
                                r
                            ),
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
                    (lty, rty) if lty.eq_ignore_span(&rty) => {}
                    ref ty => unreachable!("{:?} cannot be merged with {:?}", ty.0, ty.1),
                }
            }
            Entry::Vacant(e) => {
                println!("({}) register_type({})", depth, e.key());
                e.insert(ty);
            }
        }
    }

    pub fn this(&self) -> Option<Cow<Type>> {
        if let Some(ref this) = self.this {
            return Some(Cow::Owned(Type::Ref(Ref {
                span: DUMMY_SP,
                type_name: TsEntityName::Ident(Ident::new(this.clone().into(), DUMMY_SP)),
                type_args: None,
            })));
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
}

impl Analyzer<'_, '_> {
    /// Overrides a variable. Used for removing lazily-typed stuffs.
    pub(super) fn override_var(
        &mut self,
        kind: VarDeclKind,
        name: JsWord,
        ty: Type,
    ) -> Result<(), Error> {
        self.declare_var(ty.span(), kind, name, Some(ty), true, true)?;

        Ok(())
    }

    /// Expands
    ///
    ///   - Type alias
    pub(super) fn expand(&mut self, span: Span, ty: Type) -> ValidationResult<Type> {
        macro_rules! verify {
            ($ty:expr) => {{
                if cfg!(debug_assertions) {
                    match $ty.normalize() {
                        Type::Ref(ref s) => unreachable!("ref: {:?}", s),
                        _ => {}
                    }
                }
            }};
        }

        match ty.normalize() {
            Type::Ref(Ref {
                ref type_name,
                ref type_args,
                ..
            }) => {
                match *type_name {
                    TsEntityName::Ident(ref i) => {
                        // Check for builtin types
                        if !self.is_builtin {
                            if let Ok(ty) = builtin_types::get_type(self.libs, span, &i.sym) {
                                verify!(ty);
                                return self.expand(span, ty);
                            }
                        }

                        // Handle enum
                        if let Some(ty) = self.find_type(&i.sym) {
                            match ty.normalize() {
                                Type::Enum(..) => {
                                    if let Some(..) = *type_args {
                                        return Err(Error::NotGeneric { span });
                                    }
                                    verify!(ty);
                                    return Ok(ty.clone());
                                }

                                Type::Param(..) => {
                                    if let Some(..) = *type_args {
                                        return Err(Error::NotGeneric { span });
                                    }

                                    verify!(ty);
                                    return Ok(ty.clone());
                                }

                                Type::Interface(..) | Type::Class(..) => {
                                    // TODO: Handle type parameters
                                    verify!(ty);
                                    return Ok(ty.clone());
                                }

                                Type::Alias(Alias {
                                    type_params: None,
                                    ref ty,
                                    ..
                                }) => {
                                    verify!(ty);
                                    return Ok(*ty.clone());
                                }

                                // Expand type parameters.
                                Type::Alias(Alias {
                                    type_params: Some(ref tps),
                                    ref ty,
                                    ..
                                }) => {
                                    let tps = tps.clone();
                                    let ty = ty.clone();
                                    let ty = if let Some(i) = type_args {
                                        self.expand_type_params(i, &tps, *ty)?
                                    } else {
                                        *ty.clone()
                                    };
                                    let ty = self.expand(span, ty)?;

                                    verify!(ty);
                                    return Ok(ty.clone());
                                }

                                _ => unimplemented!("Handling result of find_type() -> {:#?}", ty),
                            }
                        } else {
                            println!("Failed to find type: {}", i.sym)
                        }
                    }

                    // Handle enum variant type.
                    //
                    //  let a: StringEnum.Foo = x;
                    TsEntityName::TsQualifiedName(box TsQualifiedName {
                        left: TsEntityName::Ident(ref left),
                        ref right,
                    }) => {
                        if left.sym == js_word!("void") {
                            return Ok(Type::any(span));
                        }

                        if let Some(ref ty) = self.scope.find_type(&left.sym) {
                            match *ty {
                                Type::Enum(..) => {
                                    return Ok(EnumVariant {
                                        span,
                                        enum_name: left.sym.clone(),
                                        name: right.sym.clone(),
                                    }
                                    .into());
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {
                        unimplemented!("TsEntityName: {:?}", type_name);
                    }
                }

                return Err(Error::NameNotFound {
                    name: type_name.clone().into(),
                    span: type_name.span(),
                });
            }

            Type::Query(QueryType {
                expr: QueryExpr::TsEntityName(ref name),
                ..
            }) => return self.type_of_ts_entity_name(span, name, None),

            _ => {}
        }

        let ty = match ty {
            Type::Union(Union { span, types }) => {
                let v = types
                    .into_iter()
                    .map(|ty| -> ValidationResult { Ok(self.expand(span, ty)?) })
                    .collect::<Result<Vec<_>, _>>()?;
                return Ok(Type::union(v));
            }
            Type::Intersection(Intersection { span, types }) => {
                return Ok(Intersection {
                    span,
                    types: types
                        .into_iter()
                        .map(|ty| -> ValidationResult { Ok(self.expand(span, ty)?) })
                        .collect::<Result<_, _>>()?,
                }
                .into());
            }

            Type::Array(Array {
                span,
                box elem_type,
            }) => {
                let elem_type = box self.expand(elem_type.span(), elem_type)?;
                return Ok(Array { span, elem_type }.into());
            }

            // type Baz = "baz"
            // let a: ["foo", "bar", Baz] = ["foo", "bar", "baz"];
            Type::Tuple(Tuple { span, types }) => {
                return Ok(Tuple {
                    span,
                    types: types
                        .into_iter()
                        .map(|v| self.expand(v.span(), v))
                        .collect::<Result<_, _>>()?,
                }
                .into());
            }

            Type::Alias(Alias {
                type_params: None,
                ty,
                ..
            }) => {
                return Ok(*ty);
            }

            Type::Function(ty::Function {
                span,
                type_params,
                params,
                ret_ty,
            }) => {
                return Ok(ty::Function {
                    span,
                    type_params,
                    params,
                    ret_ty: box self.expand(span, *ret_ty)?,
                }
                .into());
            }

            ty => ty,
        };

        Ok(ty)
    }

    pub(super) fn register_type(&mut self, name: JsWord, ty: Type) -> Result<(), Error> {
        if self.is_builtin {
            self.info.exports.types.insert(name, ty.freeze());
        } else {
            self.scope.register_type(name, ty);
        }

        Ok(())
    }

    pub fn declare_vars(&mut self, kind: VarDeclKind, pat: &Pat) -> Result<(), Error> {
        self.declare_vars_inner(kind, pat, false)
    }

    /// Updates variable list.
    ///
    /// This method should be called for function parameters including error
    /// variable from a catch clause.
    pub(super) fn declare_vars_inner(
        &mut self,
        kind: VarDeclKind,
        pat: &Pat,
        export: bool,
    ) -> Result<(), Error> {
        match *pat {
            Pat::Ident(ref i) => {
                let ty = try_opt!(i.type_ann.validate_with(self));
                let ty = try_opt!(ty.map(|ty| self.expand(pat.span(), ty)));

                let name = i.sym.clone();
                self.declare_var(
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
                        .insert(name, ty.unwrap_or(Type::any(i.span)).freeze())
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

            Pat::Invalid(..) | Pat::Expr(box Expr::Invalid(..)) => Ok(()),

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

    pub(super) fn find_var_type(&self, name: &JsWord) -> Option<Cow<Type>> {
        // println!("({}) find_var_type({})", self.scope.depth(), name);
        let mut scope = Some(&self.scope);
        while let Some(s) = scope {
            if let Some(ref v) = s.facts.vars.get(&Name::from(name)) {
                println!(
                    "({}) find_var_type({}): Handled from facts",
                    self.scope.depth(),
                    name
                );
                return Some(Cow::Borrowed(v));
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

            let mut ty = match var.ty {
                Some(ref ty) => ty.clone(),
                _ => return None,
            };

            if let Some(ref excludes) = self.scope.facts.excludes.get(&name) {
                match ty {
                    Type::Union(ty::Union { ref mut types, .. }) => {
                        for ty in types {
                            let span = (*ty).span();
                            for excluded_ty in excludes.iter() {
                                if ty.eq_ignore_name_and_span(excluded_ty) {
                                    *ty = Type::never(span)
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            return Some(Cow::Owned(ty));
        }

        None
    }

    #[inline(never)]
    pub(super) fn find_type(&self, name: &JsWord) -> Option<&Type> {
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

    pub fn declare_var(
        &mut self,
        span: Span,
        kind: VarDeclKind,
        name: JsWord,
        ty: Option<Type>,
        initialized: bool,
        allow_multiple: bool,
    ) -> Result<(), Error> {
        //println!(
        //    "({}) declare_var({}, initialized = {:?})",
        //    self.depth(),
        //    name,
        //    initialized,
        //);

        no_ref!(ty);

        match self.scope.vars.entry(name.clone()) {
            Entry::Occupied(e) => {
                if !allow_multiple {
                    return Err(Error::DuplicateName { name, span });
                }
                //println!("\tdeclare_var: found entry");
                let (k, mut v) = e.remove_entry();

                macro_rules! restore {
                    () => {{
                        self.scope.vars.insert(k, v);
                    }};
                }

                v.ty = if let Some(ty) = ty {
                    let ty = ty.generalize_lit().into_owned();

                    Some(if let Some(var_ty) = v.ty {
                        let var_ty = var_ty.generalize_lit().into_owned();

                        match ty {
                            Type::Query(..) | Type::Function(..) => {}
                            _ => {
                                let generalized_var_ty = var_ty.generalize_lit();

                                let res = self.assign(&ty, &generalized_var_ty, span);

                                if res.is_err() {
                                    v.ty = Some(var_ty);
                                    restore!();
                                    return Err(Error::RedeclaredVarWithDifferentType { span });
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

                self.scope.vars.insert(k, v);
            }
            Entry::Vacant(e) => {
                //println!("\tdeclare_var: no entry");

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

    pub fn declare_complex_vars(
        &mut self,
        kind: VarDeclKind,
        pat: &Pat,
        ty: Type,
    ) -> ValidationResult<()> {
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
                                    self.declare_complex_vars(kind, elem, ty)?;
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
                        let mut buf = vec![];
                        for ty in types.iter() {
                            match *ty.normalize() {
                                Type::Tuple(Tuple {
                                    types: ref elem_types,
                                    ..
                                }) => {
                                    buf.push(elem_types);
                                }
                                _ => {
                                    errors.push(Error::NotTuple { span: ty.span() });
                                }
                            }
                        }
                        if !errors.is_empty() {
                            return Err(Error::UnionError { span, errors });
                        }

                        for (elem, types) in elems
                            .into_iter()
                            .zip(buf.into_iter().chain(repeat(&vec![Type::undefined(span)])))
                        {
                            match *elem {
                                Some(ref elem) => {
                                    let ty = Union {
                                        span,
                                        types: types.into_iter().cloned().collect(),
                                    }
                                    .into();
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
                fn find<'a>(members: &[TypeElement], key: &PropName) -> Option<Type> {
                    let mut index_el = None;
                    // First, we search for Property
                    for m in members {
                        match *m {
                            TypeElement::Property(PropertySignature { ref type_ann, .. }) => {
                                return match *type_ann {
                                    Some(ref ty) => Some(ty.clone()),
                                    None => Some(Type::any(key.span())),
                                }
                            }

                            TypeElement::Index(IndexSignature { ref type_ann, .. }) => {
                                index_el = Some(match *type_ann {
                                    Some(ref ty) => ty.clone(),
                                    None => Type::any(key.span()),
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
                                        self.declare_complex_vars(kind, value, ty)?;
                                        return Ok(());
                                    }
                                }

                                _ => unimplemented!("handle_elems({:#?})", p),
                            }
                        }

                        return Err(Error::NoSuchProperty {
                            span,
                            prop: None,
                            prop_ty: None,
                        });
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
            declaring_prop: None,
            declaring_fn: None,
            this_class_name: None,
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
    Class,
    /// If statement, conditional expression, switch case
    Flow,
}
