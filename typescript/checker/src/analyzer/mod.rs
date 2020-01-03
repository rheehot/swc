use self::{
    control_flow::CondFacts,
    scope::{Scope, ScopeKind},
};
use crate::{errors::Error, ty::TypeRef, Exports};
use fxhash::FxHashMap;
use std::sync::Arc;
use swc_atoms::JsWord;
use swc_common::Span;

#[macro_use]
mod macros;
mod assign;
mod class;
mod control_flow;
mod enums;
mod export;
mod expr;
mod name;
mod pat;
mod props;
mod scope;
mod stmt;
mod type_facts;
mod util;

/// Note: All methods named `validate_*` return [Err] iff it's not recoverable.
#[derive(Debug)]
pub struct Analyzer<'a> {
    pub info: Info,

    scope: Scope<'a>,
}

#[derive(Debug, Default)]
pub struct Info {
    pub errors: Vec<Error>,
    pub exports: Exports<FxHashMap<JsWord, Arc<TypeRef<'static>>>>,
}

impl Analyzer<'_> {
    pub(super) fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> Ret
    where
        F: FnOnce(&mut Self) -> Ret,
    {
    }
}
