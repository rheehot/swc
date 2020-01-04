use self::{
    control_flow::CondFacts,
    scope::{Scope, ScopeKind},
};
use crate::{
    errors::Error,
    loader::Load,
    ty::{Type, TypeRef},
    Exports, Rule,
};
use fxhash::{FxHashMap, FxHashSet};
use std::sync::Arc;
use swc_atoms::JsWord;
use swc_common::Span;
use swc_common::{SourceMap, Span};
use swc_ts_builtin_types::Lib;

#[macro_use]
mod macros;
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
pub struct Analyzer<'a,'b> {
pub struct Analyzer<'a, 'b> {
    pub info: Info,

    resolved_imports: FxHashMap<JsWord, Arc<Type<'static>>>,
    errored_imports: FxHashSet<JsWord>,
    pending_exports: Vec<((JsWord, Span), Type<'static>)>,

    rule: Rule,
    libs: &'b [Lib],
    scope: Scope<'a>,

    loader: &'b dyn Load,
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

impl<'a, 'b> Analyzer<'a, 'b> {
    pub fn root(libs: &'b [Lib], rule: Rule, loader: &'b dyn Load) -> Self {
        Self {
            info: Default::default(),
            resolved_imports: Default::default(),
            errored_imports: Default::default(),
            pending_exports: Default::default(),
            rule,
            libs,
            scope: Scope::root(),
            loader,
        }
    }

    fn new(&self, scope: Scope<'a>) -> Self {
        Self {
            info: Default::default(),
            resolved_imports: Default::default(),
            errored_imports: Default::default(),
            pending_exports: Default::default(),
            rule: self.rule,
            libs: self.libs,
            scope,
            loader: self.loader,
        }
    }

    fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> Ret
    pub(super) fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> Ret
    where
        F: for<'any> FnOnce(&mut Analyzer<'any>) -> Ret,
        where
            F: for<'aa,'bb> FnOnce(&mut Analyzer<'aa,'bb>) -> Ret,
    where
        F: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>) -> Ret,
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
