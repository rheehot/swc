use backtrace::Backtrace;
use fxhash::FxHashMap;
use std::fmt::Debug;

#[derive(Debug, Default)]
pub(crate) struct DuplicateTracker {
    visited: FxHashMap<String, Backtrace>,
}

impl DuplicateTracker {
    pub fn record(&mut self, node: &dyn Debug) {
        let key = format!("{:?}", node);
        let bt = backtrace::Backtrace::new();

        self.insert(format!("{:?}", key), bt)
    }

    fn insert(&mut self, key: String, bt: Backtrace) {
        if let Some(bt1) = self.visited.get(&*key) {
            panic!("Duplicated detected:\n{:?}\n{:?}", bt1, bt)
        }

        self.visited.insert(key, bt);
    }

    pub fn record_all(&mut self, other: DuplicateTracker) {
        for (k, v) in other.visited {
            self.insert(k, v);
        }
    }
}
