pub(crate) struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    kind: ScopeKind,
}

impl Scope<'_> {
    pub const fn kind(&self) -> ScopeKind {
        self.kind
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
