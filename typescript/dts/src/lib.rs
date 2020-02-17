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
        let init = match node.init {
            Some(box Expr::Lit(..)) => node.init,
            _ => None,
        };

        VarDeclarator { init, ..node }
    }
}

impl Fold<ClassDecl> for TypeResolver {
    fn fold(&mut self, node: ClassDecl) -> ClassDecl {
        ClassDecl {
            declare: true,
            ..node.fold_children(self)
        }
    }
}

impl Fold<FnDecl> for TypeResolver {
    fn fold(&mut self, node: FnDecl) -> FnDecl {
        FnDecl {
            declare: true,
            ..node.fold_children(self)
        }
    }
}

impl Fold<TsModuleDecl> for TypeResolver {
    fn fold(&mut self, node: TsModuleDecl) -> TsModuleDecl {
        TsModuleDecl {
            declare: true,
            ..node.fold_children(self)
        }
    }
}

impl Fold<TsTypeAliasDecl> for TypeResolver {
    fn fold(&mut self, node: TsTypeAliasDecl) -> TsTypeAliasDecl {
        TsTypeAliasDecl {
            declare: true,
            ..node.fold_children(self)
        }
    }
}

impl Fold<Option<BlockStmt>> for TypeResolver {
    #[inline]
    fn fold(&mut self, _: Option<BlockStmt>) -> Option<BlockStmt> {
        None
    }
}
