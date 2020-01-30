use backtrace::Backtrace;
use fxhash::FxHashMap;
use std::{cmp::min, fmt::Debug};
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
        let bt = Backtrace::new_unresolved();

        self.insert(format!("{:?}", key), bt)
    }

    fn insert(&mut self, key: String, bt: Backtrace) {
        if let Some(bt1) = self.visited.remove(&*key) {
            let (l, r) = remove_common(bt1, bt);
            panic!("Duplicated detected:\n{}\n{:?}\n{:?}", key, l, r)
        }

        self.visited.insert(key, bt);
    }

    pub fn record_all(&mut self, other: DuplicateTracker) {
        for (k, v) in other.visited {
            self.insert(k, v);
        }
    }
}

fn remove_common(mut l: Backtrace, mut r: Backtrace) -> (Backtrace, Backtrace) {
    let (l, r) = (filter(l), filter(r));
    let (mut l, mut r): (Vec<_>, Vec<_>) = (l.into(), r.into());

    //    let mut start = 0;
    //    for i in 0..min(l.len(), r.len()) {
    //        let (lf, rf) = (&l[i], &r[i]);
    //        let (ls, rs) = (lf.symbols(), rf.symbols());
    //
    //        let mut all_ok = true;
    //
    //        for j in 0..min(ls.len(), rs.len()) {
    //            let (ls, rs) = (&ls[j], &rs[j]);
    //
    //            if ls.filename().is_some()
    //                && rs.filename().is_some()
    //                && ls.filename() == rs.filename()
    //                && ls.lineno() == rs.lineno()
    //            {
    //                all_ok = false;
    //                break;
    //            }
    //        }
    //
    //        if all_ok {
    //            start = i
    //        }
    //    }
    //    start -= 1;
    //
    //    l.drain(..start);
    //    r.drain(..start);

    (l.into(), r.into())
}

fn filter(mut bt: Backtrace) -> Backtrace {
    bt.resolve();
    let mut frames: Vec<_> = bt.into();
    let mut done = false;

    frames.retain(|frame| {
        if done {
            return false;
        }

        //
        for symbol in frame.symbols() {
            let name = if let Some(name) = symbol.name().and_then(|s| s.as_str()) {
                name
            } else {
                return true;
            };

            //            if let Some(filename) = symbol.filename() {
            //                let s = filename.to_string_lossy();
            //                if s.contains("fold.rs")
            //                    || s.contains("validator.rs")
            //                    || s.contains("vec.rs")
            //                    || s.contains("/backtrace")
            //                    || s.contains("/ast/")
            //                    || s.contains("libcore")
            //                {
            //                    return false;
            //                }
            //            }

            if name.contains("Module") {
                done = true;
                // Last one
                return true;
            }
        }

        true
    });

    frames.into()
}
