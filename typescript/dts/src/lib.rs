#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use swc_common::{Fold, FoldWith};
use swc_ecma_ast::*;
use swc_ts_checker::ModuleTypeInfo;

#[derive(Debug)]
pub struct TypeResolver {
    pub types: ModuleTypeInfo,
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
