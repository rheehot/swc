use self::{
    control_flow::CondFacts,
    scope::{Scope, ScopeKind},
};
use crate::{
    errors::Error,
    ty::{Type, TypeRef},
    Exports, Rule,
};
use fxhash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;
use std::sync::Arc;
use swc_atoms::JsWord;
use swc_common::Span;
use swc_common::{SourceMap, Span};
use swc_ts_builtin_types::Lib;

mod assign;
mod class;
mod control_flow;
mod enums;
mod export;
mod expr;
mod import;
mod name;
mod pat;
mod props;
mod scope;
mod stmt;
mod type_facts;
mod util;

/// Note: All methods named `validate_*` return [Err] iff it's not recoverable.
#[derive(Debug)]
pub(crate) struct Analyzer<'a> {
    pub info: Info,
    resolved_imports: FxHashMap<JsWord, Arc<Type<'static>>>,
    errored_imports: FxHashSet<JsWord>,
    pending_exports: Vec<((JsWord, Span), Type<'static>)>,

    declaring: SmallVec<[JsWord; 8]>,

    rule: Rule,
    libs: &'a [Lib],
    scope: Scope<'a>,
}

#[derive(Debug, Clone, Default)]
pub struct Info {
    pub errors: Vec<Error>,
    pub exports: Exports<FxHashMap<JsWord, Arc<TypeRef<'static>>>>,
}

impl Analyzer<'_> {
fn _assert_types() {
    fn is_sync<T: Sync>() {}
    fn is_send<T: Send>() {}
    is_sync::<Info>();
    is_send::<Info>();
}

impl<'a> Analyzer<'a> {
    pub fn root(libs: &'a [Lib], rule: Rule) -> Self {
        Self {
            info: Default::default(),
            resolved_imports: Default::default(),
            errored_imports: Default::default(),
            pending_exports: Default::default(),
            declaring: Default::default(),
            rule,
            libs,
            scope: Scope::root(),
        }
    }

    fn new(&self, scope: Scope<'a>) -> Self {
        Self {
            info: Default::default(),
            resolved_imports: Default::default(),
            errored_imports: Default::default(),
            pending_exports: Default::default(),
            declaring: Default::default(),
            rule: self.rule,
            libs: self.libs,
            scope,
        }
    }

    fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> Ret
    pub(super) fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> Ret
    where
        F: for<'any> FnOnce(&mut Analyzer<'any>) -> Ret,
    {
        let child_scope = Scope::new(&self.scope, kind, facts);
        let (ret, info) = {
            let mut child = self.new(child_scope);

            let ret = op(&mut child);

            (ret, child.info)
        };

        self.info.errors.extend(info.errors);
        assert!(info.exports.types.is_empty(), "child cannot export a type");
        assert!(
            info.exports.vars.is_empty(),
            "child cannot export a variable"
        );

        ret
    }
}
