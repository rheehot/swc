#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use swc_common::Fold;
use swc_ecma_ast::*;
use swc_ts_checker::{Checker, ModuleTypeInfo};

#[derive(Debug)]
struct TypeResolver {
    checker: Checker,
    types: ModuleTypeInfo,
}

impl Fold<Module> for TypeResolver {
    fn fold(&mut self, node: Module) -> Module {}
}
