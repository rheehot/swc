#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use swc_common::{Fold, FoldWith};
use swc_ecma_ast::*;
use swc_ts_checker::ModuleTypeInfo;

pub fn generate_dts(module: Module, info: ModuleTypeInfo) -> Module {
    module.fold_with(&mut TypeResolver { types: info })
}

#[derive(Debug)]
struct TypeResolver {
    types: ModuleTypeInfo,
}

impl Fold<VarDecl> for TypeResolver {
    fn fold(&mut self, node: VarDecl) -> VarDecl {
        let node = node.fold_children(self);

        VarDecl {
            declare: true,
            ..node
        }
    }
}

impl Fold<VarDeclarator> for TypeResolver {
    fn fold(&mut self, node: VarDeclarator) -> VarDeclarator {
        VarDeclarator { init: None, ..node }
    }
}
