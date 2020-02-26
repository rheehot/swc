use super::Analyzer;
use crate::{
    analyzer::scope::Scope,
    builtin_types,
    ty::{
        self, Alias, Array, CallSignature, Conditional, FnParam, IndexedAccessType, Interface,
        Mapped, Operator, PropertySignature, Ref, Tuple, Type, TypeElement, TypeLit, TypeOrSpread,
        TypeParam, TypeParamDecl, TypeParamInstantiation, Union,
    },
    ValidationResult,
};
use bitflags::_core::mem::take;
use fxhash::{FxHashMap, FxHashSet};
use swc_atoms::{js_word, JsWord};
use swc_common::{Fold, FoldWith, Span, Spanned, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_utils::Id;

impl Analyzer<'_, '_> {
    /// TODO: implement
    pub(super) fn infer_arg_types(
        &mut self,
        type_params: &[TypeParam],
        params: &[FnParam],
        args: &[TypeOrSpread],
    ) -> ValidationResult<TypeParamInstantiation> {
        let mut inferred = FxHashMap::default();

        // TODO: Handle optional parameters
        // TODO: Convert this to error.
        assert!(args.len() >= params.len());

        for (p, arg) in params.iter().zip(args) {
            match &p.ty {
                Type::Param(TypeParam { ref name, .. }) => {
                    assert_eq!(
                        arg.spread, None,
                        "argument type inference for spread argument is not implemented yet"
                    );

                    // Very simple case.
                    inferred.insert(name, arg.ty.clone());
                    // function foo<T>(a: T) {}
                }

                _ => {}
            }
        }

        let mut params = Vec::with_capacity(type_params.len());
        for type_param in type_params {
            if let Some(ty) = inferred.remove(&type_param.name) {
                params.push(ty);
            } else {
                log::warn!(
                    "infer_type_args: falling back to unknown type parameter: {:?}",
                    type_param
                );

                // TODO: Fix this and implement full type inference
                params.push(type_param.clone().into());
            }
        }

        Ok(TypeParamInstantiation {
            span: DUMMY_SP,
            params,
        })
    }

    pub(super) fn expand_type_params(
        &mut self,
        i: &TypeParamInstantiation,
        params: &[TypeParam],
        ty: Type,
    ) -> ValidationResult {
        self.expand_type_params_inner(i, params, ty, false)
    }

    /// if `fully` is true, interfaces are converted into type literal and
    /// resolved.
    ///
    ///
    /// TODO: Handle operators like keyof.
    ///  e.g.
    ///    Convert
    ///      type BadNested<T> = {
    ///          x: T extends number ? T : string;
    ///      };
    ///      T extends {
    ///          [K in keyof BadNested<infer P>]: BadNested<infer P>[K];
    ///      } ? P : never;
    ///   into
    ///      T extends {
    ///          x: infer P extends number ? infer P : string;
    ///      } ? P : never
    fn expand_type_params_inner(
        &mut self,
        type_args: &TypeParamInstantiation,
        params: &[TypeParam],
        ty: Type,
        fully: bool,
    ) -> ValidationResult {
        let mut ty = ty.fold_with(&mut GenericExpander {
            analyzer: self,
            params,
            i: type_args,
            fully,
            dejavu: Default::default(),
        });

        Ok(ty)
    }

    /// Returns `Some(true)` if `child` extends `parent`.
    fn extends(&self, child: &Type, parent: &Type) -> Option<bool> {
        match child {
            Type::Ref(..) => return None,
            _ => {}
        }
        match parent {
            Type::Ref(..) => return None,
            _ => {}
        }

        let span = child.span();

        match self.assign(parent, child, span) {
            Ok(()) => Some(true),
            _ => None,
        }
    }
}

struct GenericExpander<'a, 'b, 'c> {
    analyzer: &'a Analyzer<'b, 'c>,
    params: &'a [TypeParam],
    i: &'a TypeParamInstantiation,
    /// Expand fully?
    fully: bool,
    dejavu: FxHashSet<JsWord>,
}

impl Fold<Type> for GenericExpander<'_, '_, '_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        let old_fully = self.fully;
        self.fully |= match ty {
            Type::Mapped(..) => true,
            _ => false,
        };

        match ty {
            Type::Ref(Ref {
                span,
                type_name: TsEntityName::Ident(Ident { ref sym, .. }),
                ref type_args,
                ..
            }) => {
                if *sym == js_word!("Array") {
                    return Type::Array(Array {
                        span,
                        elem_type: box type_args
                            .as_ref()
                            .and_then(|args| args.params.iter().next().cloned())
                            .unwrap_or_else(|| Type::any(span)),
                    });
                }

                if self.dejavu.contains(sym) {
                    log::debug!("Dejavu: {}", sym);
                    return ty;
                }

                log::info!("Ref: {}", sym);

                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == *sym {
                        assert_eq!(*type_args, None);

                        return self.i.params[idx].clone();
                    }
                }

                if self.fully {
                    // Check for builtin types
                    if !self.analyzer.is_builtin {
                        if let Ok(ty) = builtin_types::get_type(self.analyzer.libs, span, sym) {
                            let ty = ty.fold_with(self);
                            return ty;
                        }
                    }
                }

                if self.fully {
                    if let Some(types) = self.analyzer.find_type(sym) {
                        log::info!(
                            "Found {} items from {}",
                            types.clone().into_iter().count(),
                            sym
                        );

                        for t in types {
                            match t.normalize() {
                                Type::Alias(alias) => {
                                    if let Some(type_params) = &alias.type_params {
                                        if let Some(type_args) = &type_args {
                                            let mut v = GenericExpander {
                                                analyzer: self.analyzer,
                                                params: &type_params.params,
                                                i: type_args,
                                                fully: self.fully,
                                                dejavu: {
                                                    let mut v = self.dejavu.clone();
                                                    v.insert(sym.clone());
                                                    v
                                                },
                                            };

                                            return *alias.ty.clone().fold_with(&mut v);
                                        }
                                    } else {
                                        return *alias.ty.clone();
                                    }
                                }

                                Type::Interface(i) => {
                                    // TODO: Handle super
                                    if !i.extends.is_empty() {
                                        log::error!(
                                            "not yet implemented: expanding interface which has a \
                                             parent"
                                        );
                                        return ty;
                                    }

                                    let members = if let Some(type_params) = &i.type_params {
                                        let type_args = if let Some(type_args) = &type_args {
                                            type_args
                                        } else {
                                            return ty;
                                        };

                                        let mut v = GenericExpander {
                                            analyzer: self.analyzer,
                                            params: &type_params.params,
                                            i: type_args,
                                            fully: self.fully,
                                            dejavu: {
                                                let mut v = self.dejavu.clone();
                                                v.insert(sym.clone());
                                                v
                                            },
                                        };

                                        i.body.clone().fold_with(&mut v)
                                    } else {
                                        i.body.clone()
                                    };

                                    return Type::TypeLit(TypeLit {
                                        span: i.span,
                                        members,
                                    });
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }

            Type::Ref(..) => return ty,

            Type::Param(mut param) => {
                param = param.fold_with(self);

                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == param.name {
                        match self.i.params[idx].clone().normalize() {
                            Type::Param(..) => {}
                            _ => return self.i.params[idx].clone(),
                        }
                    }
                }

                if param.constraint.is_some() && is_literals(&param.constraint.as_ref().unwrap()) {
                    return *param.constraint.clone().unwrap();
                }

                if param.constraint.is_some()
                    && match **param.constraint.as_ref().unwrap() {
                        Type::Keyword(..) => true,
                        Type::Ref(..) => true,
                        Type::TypeLit(..) => true,
                        _ => false,
                    }
                {
                    return *param.constraint.clone().unwrap();
                }

                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == param.name {
                        match self.i.params[idx].clone().normalize() {
                            Type::Param(..) => return self.i.params[idx].clone(),
                            _ => {}
                        }
                    }
                }

                return Type::Param(param);
            }

            // Alias returns other than self.
            Type::Alias(mut alias) => {
                alias = alias.fold_with(self);

                if let Some(..) = &alias.type_params {
                    // TODO: Handle unresolved type parameter
                    log::warn!("An type alias has type parameters. It may not be fully expanded.");
                }
                return *alias.ty;
            }

            Type::Interface(mut i) if self.fully => {
                i = i.fold_with(self);

                if let Some(..) = &i.type_params {
                    log::error!("An interface has type parameters. It may not be fully expanded.");
                }

                // TODO: Handle super
                if !i.extends.is_empty() {
                    log::error!("not yet implemented: expanding interface which has a parent");
                    return Type::Interface(i);
                }

                return Type::TypeLit(TypeLit {
                    span: i.span,
                    members: i.body,
                });
            }

            Type::Class(mut c) => {
                c = c.fold_with(self);

                if let Some(..) = &c.type_params {
                    log::error!("A class has type parameters. It may not be fully expanded.");
                }

                return Type::Class(c);
            }

            Type::Conditional(mut c) => {
                c = c.fold_with(self);

                // if let Some(v) = self.analyzer.extends(&c.check_type, &c.extends_type) {
                //     return if v { *c.true_type } else { *c.false_type };
                // }

                return Type::Conditional(c);
            }

            Type::Mapped(mut m @ Mapped { ty: Some(..), .. }) => {
                m = m.fold_with(self);

                m.ty = match m.ty {
                    Some(box Type::IndexedAccessType(IndexedAccessType {
                        span,
                        readonly,
                        obj_type,
                        index_type,
                    })) => {
                        match *obj_type {
                            Type::TypeLit(TypeLit { span, members, .. })
                                if members.iter().all(|m| match m {
                                    TypeElement::Property(_) => true,
                                    _ => false,
                                }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for m in members {
                                    match m {
                                        ty::TypeElement::Property(p) => {
                                            //
                                            new_members.push(ty::TypeElement::Property(p));
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                return Type::TypeLit(TypeLit {
                                    span,
                                    members: new_members,
                                });
                            }

                            _ => Some(box Type::IndexedAccessType(IndexedAccessType {
                                span,
                                readonly,
                                obj_type,
                                index_type,
                            })),
                        }
                    }
                    _ => m.ty,
                };

                if let Some(constraint) = &m.type_param.constraint {
                    match &**constraint {
                        Type::Operator(Operator {
                            span,
                            op: TsTypeOperatorOp::KeyOf,
                            ty,
                        }) => match &**ty {
                            Type::Keyword(..) => return *ty.clone(),
                            Type::TypeLit(TypeLit { span, members, .. })
                                if members.iter().all(|m| match m {
                                    TypeElement::Property(_) => true,
                                    TypeElement::Method(_) => true,
                                    _ => false,
                                }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for member in members {
                                    match member {
                                        ty::TypeElement::Method(method) => {
                                            new_members.push(ty::TypeElement::Property(
                                                PropertySignature {
                                                    span: method.span,
                                                    readonly: method.readonly,
                                                    key: method.key.clone(),
                                                    computed: method.computed,
                                                    optional: method.optional,
                                                    params: vec![],
                                                    type_ann: m.ty.clone().map(|v| *v),
                                                    type_params: None,
                                                },
                                            ));
                                        }
                                        ty::TypeElement::Property(p) => {
                                            let mut p = p.clone();
                                            if let Some(ty) = &m.ty {
                                                p.type_ann = Some(*ty.clone());
                                            }
                                            //
                                            new_members.push(ty::TypeElement::Property(p));
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                return Type::TypeLit(TypeLit {
                                    span: *span,
                                    members: new_members,
                                });
                            }
                            _ => {}
                        },

                        _ => {}
                    }
                }

                return Type::Mapped(m);
            }

            Type::This(..) | Type::Keyword(..) | Type::TypeLit(..) | Type::Lit(..) => {
                return ty.fold_children(self)
            }

            Type::Query(..)
            | Type::Operator(..)
            | Type::Tuple(..)
            | Type::Infer(..)
            | Type::Import(..)
            | Type::Predicate(..)
            | Type::Array(..)
            | Type::Union(..)
            | Type::Intersection(..)
            | Type::IndexedAccessType(..)
            | Type::Function(..)
            | Type::Constructor(..)
            | Type::Method(..)
            | Type::Enum(..)
            | Type::EnumVariant(..)
            | Type::Interface(..)
            | Type::Namespace(..)
            | Type::Module(..)
            | Type::ClassInstance(..)
            | Type::Mapped(..) => return ty.fold_children(self),

            Type::Static(s) => return s.ty.clone().fold_with(self),
            Type::Arc(a) => return (*a).clone().fold_with(self),
        }

        ty
    }
}

/// This method returns true for types like `'foo'` and `'foo' | 'bar'`.
pub(super) fn is_literals(ty: &Type) -> bool {
    match ty.normalize() {
        Type::Lit(_) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|v| is_literals(v)),
        _ => false,
    }
}
