use super::{control_flow::CondFacts, scope::ScopeKind, Analyzer};
use crate::errors::Error;
use std::iter::once;

impl Analyzer<'_> {
    pub(super) fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> Ret
    where
        F: FnOnce(&mut Self) -> Ret,
    {
    }
}

pub trait ResultExt<T, E>: Into<Result<T, E>> {
    fn store<V>(self, to: &mut V) -> Option<T>
    where
        V: Extend<E>,
    {
        match self.into() {
            Ok(val) => Some(val),
            Err(e) => {
                to.extend(once(e));
                None
            }
        }
    }
}

impl<T, E> ResultExt<T, E> for Result<T, E> {}

/// Simple utility to check (l, r) and (r, l) with same code.
#[derive(Debug, Clone, Copy)]
pub(super) struct Comparator<T>
where
    T: Copy,
{
    pub left: T,
    pub right: T,
}

impl<T> Comparator<T>
where
    T: Copy,
{
    pub fn take<F, R>(&self, mut op: F) -> Option<R>
    where
        F: FnMut(T, T) -> Option<R>,
    {
        op(self.left, self.right).or_else(|| op(self.right, self.left))
    }

    pub fn both<F>(&self, mut op: F) -> bool
    where
        F: FnMut(T) -> bool,
    {
        op(self.left) && op(self.right)
    }

    pub fn any<F>(&self, mut op: F) -> bool
    where
        F: FnMut(T) -> bool,
    {
        op(self.left) || op(self.right)
    }
}
