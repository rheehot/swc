use crate::{analyzer::control_flow::CondFacts, ty::Type};
use fxhash::FxHashMap;

pub(crate) struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
    pub(super) vars: FxHashMap<JsWord, Type<'static>>,
    pub(super) types: FxHashMap<JsWord, Type<'static>>,
    facts: CondFacts,
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
            vars: Default::default(),
            types: Default::default(),
            facts,
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
