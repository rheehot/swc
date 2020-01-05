use self::{
    control_flow::CondFacts,
    scope::{Scope, ScopeKind},
};
use crate::{
    analyzer::{props::ComputedPropMode, util::ResultExt},
    errors::Error,
    loader::Load,
    ty::Type,
    validator::Validate,
    Exports, ImportInfo, Rule,
};
use fxhash::{FxHashMap, FxHashSet};
use std::{path::PathBuf, sync::Arc};
use swc_atoms::JsWord;
use swc_common::Span;
use swc_common::{SourceMap, Span};
use swc_common::{Span, Visit};
use swc_ecma_ast::Decorator;
use swc_ts_builtin_types::Lib;

#[macro_use]
mod macros;
mod assign;
mod class;
mod control_flow;
mod convert;
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
mod type_params;
mod util;

/// Note: All methods named `validate_*` return [Err] iff it's not recoverable.
#[derive(Debug)]
pub(crate) struct Analyzer<'a> {
pub struct Analyzer<'a,'b> {
pub struct Analyzer<'a, 'b> {
    pub info: Info,

    resolved_imports: FxHashMap<JsWord, Arc<Type>>,
    errored_imports: FxHashSet<JsWord>,
    pending_exports: Vec<((JsWord, Span), Type)>,

    rule: Rule,
    libs: &'b [Lib],
    scope: Scope<'a>,

    loader: &'b dyn Load,

    allow_ref_declaring: bool,
    computed_prop_mode: ComputedPropMode,
    is_builtin: bool,
}

#[derive(Debug, Clone, Default)]
pub struct Info {
    pub errors: Vec<Error>,
    pub exports: Exports<FxHashMap<JsWord, Arc<Type>>>,
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
        Self::new_inner(libs, rule, loader, Scope::root(), false)
    }

    pub(crate) fn for_builtin() -> Self {
        Self::new_inner(&[], Default::default(), &NoopLoader, Scope::root(), true)
    }

    fn new(&self, scope: Scope<'a>) -> Self {
        Self::new_inner(self.libs, self.rule, self.loader, scope, false)
    }

    fn new_inner(
        libs: &'b [Lib],
        rule: Rule,
        loader: &'b dyn Load,
        scope: Scope<'a>,
        is_builtin: bool,
    ) -> Self {
        Self {
            info: Default::default(),
            resolved_imports: Default::default(),
            errored_imports: Default::default(),
            pending_exports: Default::default(),
            rule,
            libs,
            scope,
            loader,
            allow_ref_declaring: false,
            computed_prop_mode: ComputedPropMode::Object,
            is_builtin,
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

struct NoopLoader;
impl Load for NoopLoader {
    fn load(
        &self,
        base: Arc<PathBuf>,
        import: &ImportInfo,
    ) -> Result<Exports<FxHashMap<JsWord, Arc<Type>>>, Error> {
        unreachable!("builtin module should not import other moduel")
    }
}

/// Done
impl Visit<Decorator> for Analyzer<'_, '_> {
    fn visit(&mut self, d: &Decorator) {
        self.visit_expr(&d.expr).store(&mut self.info.errors);
    }
}
