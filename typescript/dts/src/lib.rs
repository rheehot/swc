#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]

use std::collections::hash_map::Entry;
use swc_common::{Fold, FoldWith};
use swc_ecma_ast::*;
use swc_ts_checker::{ty::Type, ModuleTypeInfo};

pub fn generate_dts(module: Module, info: ModuleTypeInfo) -> Module {
    module.fold_with(&mut TypeResolver { info })
}

#[derive(Debug)]
struct TypeResolver {
    info: ModuleTypeInfo,
}

impl TypeResolver {}

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
    fn fold(&mut self, mut node: VarDeclarator) -> VarDeclarator {
        match node.init {
            Some(box Expr::Lit(Lit::Null(..))) => {
                node.init = None;
            }
            Some(box Expr::Lit(..)) => {}
            _ => {
                node.init = None;
            }
        };

        match node.name {
            Pat::Ident(ref mut node) => {
                if let Some(ty) = self.info.vars.remove(nodet.sym.clone()).map(From::from) {
                    node.type_ann = Some(TsTypeAnn {
                        span: Default::default(),
                        type_ann: box ty,
                    });
                }
            }
            _ => {}
        }

        if node.init.is_none() {
            node.name = node.name.fold_with(self);
        }

        node
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
