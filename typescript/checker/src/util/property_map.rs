use crate::Id;
use fxhash::FxHashMap;
use ordered_float::OrderedFloat;
use swc_atoms::JsWord;
use swc_common::{FoldWith, DUMMY_SP};
use swc_ecma_ast::{ComputedPropName, Expr, Ident, Prop, PropName, Str};

/// **Note**: this struct ignores span of key.
#[derive(Debug)]
pub struct PropertyMap<V> {
    inner: Vec<(PropName, V)>,
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
        let expr = PropName::Computed(ComputedPropName {
            span: DUMMY_SP,
            expr: box expr.clone().fold_with(&mut super::SpanRemover),
        });

        for (k, v) in self.inner {
            if k == expr {
                return Some(&v);
            }
        }

        None
    }

    pub fn get_prop_anme(&self, p: &PropName) -> Option<&V> {
        let expr = p.clone().fold_with(&mut super::SpanRemover);

        for (k, v) in self.inner {
            if k == expr {
                return Some(&v);
            }
        }

        None
    }

    pub fn insert(&mut self, key: PropName, v: V) {
        let key = key.fold_with(&mut super::SpanRemover);
        self.inner.push((key, v));
    }
use swc_ecma_ast::{Expr, Ident, Str};

/// **Note**: this struct ignores span of key.
#[derive(Defualt)]
pub struct PropertyMap<V> {
    inner: FxHashMap<PropKey, V>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum PropKey {
    Str(JsWord),
    Ident(Id),
    Num(OrderedFloat<f64>),
    Expr(Expr),
}

impl<V> PropertyMap<V> {
    pub fn get(&self, key: &Expr) -> Option<&V> {}
}
