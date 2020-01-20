use backtrace::Backtrace;
use fxhash::FxHashMap;
use std::fmt::Debug;
use swc_common::Spanned;

#[derive(Debug, Default)]
pub(crate) struct DuplicateTracker {
    visited: FxHashMap<String, Backtrace>,
}

impl DuplicateTracker {
    pub fn record<N>(&mut self, node: &N)
    where
        N: Debug + Spanned,
    {
        if node.span().is_dummy() {
            return;
        }

        let key = format!("{:?}", node);
        let bt = backtrace::Backtrace::new();

        self.insert(format!("{:?}", key), bt)
    }

    fn insert(&mut self, key: String, bt: Backtrace) {
        if let Some(bt1) = self.visited.get(&*key) {
            panic!("Duplicated detected:\n{}\n{:?}\n{:?}", key, bt1, bt)
        }

        self.visited.insert(key, bt);
    }

    pub fn record_all(&mut self, other: DuplicateTracker) {
        for (k, v) in other.visited {
            self.insert(k, v);
        }
    }
}
