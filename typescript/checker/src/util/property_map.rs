use crate::Id;
use fxhash::FxHashMap;
use ordered_float::OrderedFloat;
use swc_atoms::JsWord;
use swc_common::{FoldWith, DUMMY_SP};
use swc_ecma_ast::{ComputedPropName, Expr, Ident, Prop, PropName, Str};

/// **Note**: this struct ignores span of key.
#[derive(Debug)]
pub struct PropertyMap<V> {
    inner: FxHashMap<PropName, V>,
}
impl<V> Default for PropertyMap<V> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<V> PropertyMap<V> {
    pub fn get(&self, expr: &Expr) -> Option<&V> {
        println!("expr");

        let expr = PropName::Computed(ComputedPropName {
            span: DUMMY_SP,
            expr: box expr.clone().fold_with(&mut super::SpanRemover),
        });

        self.inner.get(&expr)
    }

    pub fn get_prop_name(&self, p: &PropName) -> Option<&V> {
        let p = p.clone().fold_with(&mut super::SpanRemover);

        println!("p: {:?}", p);

        if let Some(v) = self.inner.get(&p) {
            println!("Matched");
            Some(v)
        } else {
            println!("Not matched");

            None
        }
    }

    pub fn insert(&mut self, key: PropName, v: V) {
        let key = key.fold_with(&mut super::SpanRemover);
        println!("insert: {:?}", key);
        self.inner.insert(key, v);
    }
use swc_ecma_ast::{Expr, Ident, Str};
use swc_common::DUMMY_SP;
use swc_ecma_ast::{ComputedPropName, Expr, Ident, Prop, PropName, Str};

/// **Note**: this struct ignores span of key.
#[derive(Debug, Default)]
pub struct PropertyMap<V> {
    inner: Vec<(PropName, V)>,
}

impl<V> PropertyMap<V> {
    pub fn get(&self, expr: &Expr) -> Option<&V> {
        let expr = PropName::Computed(ComputedPropName {
            span: DUMMY_SP,
            expr: box expr.clone().fold_with(&mut super::SpanRemover),
        });

        for (k, v) in self.inner {
            if k == expr {
                return Some(&V);
            }
        }

        None
    }

    pub fn get_prop_anme(&self, p: &PropName) -> Option<&V> {
        let expr = p.clone().fold_with(&mut super::SpanRemover);

        for (k, v) in self.inner {
            if k == expr {
                return Some(&V);
            }
        }

        None
    }

    pub fn insert(&mut self, key: PropName, v: V) {
        let key = key.fold_with(&mut super::SpanRemover);
        self.inner.insert(key, v);
    }
}
