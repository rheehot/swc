use swc_common::Fold;
use swc_ecma_ast::*;
use swc_ts_checker::{Checker, ModuleTypeInfo};

#[derive(Debug)]
struct TypeResolver<'a> {
    checker: Checker<'a>,
    types: ModuleTypeInfo,
}

impl Fold<Module> for TypeResolver {
    fn fold(&mut self, node: Module) -> Module {}
}
