//! Dead code elimination for types.

use fxhash::FxHashSet;
use swc_atoms::JsWord;
use swc_common::{Visit, VisitWith};
use swc_ecma_ast::Ident;
use swc_ts_checker::ModuleTypeInfo;

pub fn get_used(info: &ModuleTypeInfo) -> FxHashSet<JsWord> {
    let mut used = FxHashSet::default();

    for (_, v) in info.vars.iter() {
        track(&mut used, v.normalize());
        println!("track var: {:?}", v.normalize());
    }

    for (_, types) in info.types.iter() {
        for ty in types {
            println!("track: {:?}", ty.normalize());
            track(&mut used, ty.normalize());
        }
    }

    println!("Used: {:?}", used);

    used
}

fn track<T>(used: &mut FxHashSet<JsWord>, node: &T)
where
    T: for<'any> VisitWith<Tracker<'any>>,
{
    let mut v = Tracker { used };
    node.visit_with(&mut v);
}

#[derive(Debug)]
struct Tracker<'a> {
    used: &'a mut FxHashSet<JsWord>,
}

impl Visit<JsWord> for Tracker<'_> {
    fn visit(&mut self, node: &JsWord) {
        println!("used: {}", node);

        self.used.insert(node.clone());
    }
}
