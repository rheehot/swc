use crate::ty::Type;
use fxhash::FxHashMap;
use std::{collections::hash_map::Entry, hash::Hash};
use super::name::Name;;

/// Conditional facts
#[derive(Debug, Clone, Default)]
pub(crate) struct CondFacts {
    pub facts: FxHashMap<Name, TypeFacts>,
    pub vars: FxHashMap<Name, Type<'static>>,
    pub excludes: FxHashMap<Name, Vec<Type<'static>>>,
    pub types: FxHashMap<JsWord, Type<'static>>,
}

impl CondFacts {
    fn extend(&mut self, other: Self) {
        self.facts.extend(other.facts);
        self.vars.extend(other.vars);
        self.types.extend(other.types);
    }

    fn or<K, T>(mut map: FxHashMap<K, T>, map2: FxHashMap<K, T>) -> FxHashMap<K, T>
    where
        K: Eq + Hash,
        T: Merge,
    {
        for (k, v) in map2 {
            match map.entry(k) {
                Entry::Occupied(mut e) => {
                    e.get_mut().or(v);
                }
                Entry::Vacant(e) => {
                    e.insert(v);
                }
            }
        }

        map
    }
}
