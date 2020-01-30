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

fn remove_common(l: Backtrace, r: Backtrace) -> (Backtrace, Backtrace) {
    let (l, r) = (filter(l), filter(r));
    let (mut l, mut r): (Vec<_>, Vec<_>) = (l.into(), r.into());

    //    let mut start = 0;
    //    for i in 0..min(l.len(), r.len()) {
    //        let (lf, rf) = (&l[i], &r[i]);
    //        let (ls, rs) = (lf.symbols(), rf.symbols());
    //
    //        let mut eq = true;
    //
    //        if ls.len() == rs.len() {
    //            for j in 0..ls.len() {
    //                let (ls, rs) = (&ls[j], &rs[j]);
    //
    //                if ls.filename().is_none()
    //                    || rs.filename().is_none()
    //                    || ls.lineno().is_none()
    //                    || rs.lineno().is_none()
    //                    || ls.filename() != rs.filename()
    //                    || ls.lineno() != rs.lineno()
    //                {
    //                    eq = false;
    //                    break;
    //                }
    //            }
    //        } else {
    //            eq = false;
    //        }
    //
    //        if eq {
    //            start = i
    //        }
    //    }
    //    start -= 1;

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

        let symbols = frame.symbols();
        let len = symbols.len();
        for symbol in symbols {
            let name = if let Some(name) = symbol.name().and_then(|s| s.as_str()) {
                name
            } else {
                return false;
            };

            if let Some(filename) = symbol.filename() {
                let s = filename.to_string_lossy();

                if s.contains("backtrace")
                    || s.contains("libcore")
                    || s.contains("libstd")
                    || s.contains("/libtest/")
                    || s.contains("/rustc/")
                    || s.contains("libpanic_unwind/")
                {
                    return false;
                }

                if len == 1 {
                    if s.contains("scoped-tls") {}

                    if s.contains("/ast/") {
                        return false;
                    }

                    if s.contains("common") && s.ends_with("/fold.rs") {
                        return false;
                    }

                    if s.contains("checker") && s.ends_with("/validator.rs") {
                        return false;
                    }
                }

                println!("({}) Filename: {}", len, s);
            }

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
