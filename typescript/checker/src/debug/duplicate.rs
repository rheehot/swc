use backtrace::Backtrace;
use fxhash::FxHashMap;
use std::fmt::Debug;

#[derive()]
pub(crate) struct DuplicateTracker {
    visited: FxHashMap<String, Backtrace>,
}

impl DuplicateTracker {
    pub fn record(&mut self, node: &dyn Debug) {
        let key = format!("{:?}", node);
        let bt = backtrace::Backtrace::new();

        if let Some(bt1) = self.visited.get(&*key) {
            panic!("Duplicated detected:\n{:?}\n{:?}", bt1, bt)
        }

        self.visited.insert(key, bt);
    }
}
