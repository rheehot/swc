use swc_common::{Fold, FoldWith, DUMMY_SP};
use swc_ecma_ast::*;

/// Handles
///
/// ```ts
/// 
/// foo();
/// bar();
/// bar() {}
/// ```
#[derive(Debug, Default)]
pub(crate) struct AmbientFunctionHandler {
    last_ambient_name: Option<Ident>,
}

impl Fold<Stmt> for AmbientFunctionHandler {
    fn fold(&mut self, mut node: Stmt) -> Stmt {
        node = node.fold_children(self);

        match node {
            Stmt::Decl(Decl::Fn(ref decl)) => {
                if decl.function.body.is_none() {
                    self.last_ambient_name = Some(decl.ident.clone());
                } else {
                    let name = self.last_ambient_name.take();
                    if let Some(prev_name) = name {
                        if prev_name.sym == decl.ident.sym {
                            return Stmt::Empty(EmptyStmt { span: DUMMY_SP });
                        }
                    }
                }
            }
            _ => {}
        }

        node
    }
}

impl Fold<TsModuleDecl> for AmbientFunctionHandler {
    #[inline(always)]
    fn fold(&mut self, node: TsModuleDecl) -> TsModuleDecl {
        node
    }
}
