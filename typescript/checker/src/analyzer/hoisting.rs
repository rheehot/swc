use crate::{ty::TypeParam, ValidationResult};
use bitflags::_core::mem::{replace, take};
use fxhash::FxHashSet;
use petgraph::{
    algo::toposort,
    graph::DiGraph,
    graphmap::{DiGraphMap, GraphMap},
    stable_graph::StableDiGraph,
};
use swc_atoms::{JsWord, JsWordStaticSet};
use swc_common::{Visit, VisitWith};
use swc_ecma_ast::*;
use swc_ecma_utils::{find_ids, ident::IdentLike, DestructuringFinder, Id, StmtLike};

/// Returns the order of evaluation. This methods is used to handle hoisting
/// properly.
///
/// # Exmaple
///
/// The method will return `[1, 0]` for the code below.
///
/// ```js
/// function foo() {
///     return bar();
/// }
///
/// function bar (){
///     return 1;
/// }
/// ```
pub(super) fn order<T>(nodes: &[T]) -> Vec<usize>
where
    T: StmtLike + for<'any> VisitWith<StmtDependencyFinder<'any>>,
{
    let mut order = (0..nodes.len()).collect();

    let mut ids = Vec::with_capacity(8);
    let mut dependencies = Vec::with_capacity(16);

    for node in nodes.iter() {
        ids.clear();
        dependencies.clear();

        {
            let mut v = StmtDependencyFinder {
                ids: &mut ids,
                dependencies: &mut dependencies,
            };

            node.visit_with(&mut v);
        }

        //
    }

    order
}

pub(super) struct DependencyFinder {
    /// Identifiers contained in a declaration.
use crate::{analyzer::Analyzer, ty::TypeParam, ValidationResult};
use bitflags::_core::mem::{replace, take};
use fxhash::{FxHashMap, FxHashSet};
use petgraph::{
    algo::toposort,
    graph::DiGraph,
    graphmap::{DiGraphMap, GraphMap},
    stable_graph::StableDiGraph,
    visit::DfsPostOrder,
};
use swc_atoms::{JsWord, JsWordStaticSet};
use swc_common::{Visit, VisitWith};
use swc_ecma_ast::*;
use swc_ecma_utils::{find_ids, ident::IdentLike, DestructuringFinder, Id, StmtLike};

/// Structs to reuse vector / hash maps.
#[derive(Debug, Default)]
pub(super) struct HoistingWs {
    stmts: StmtWs,
    type_params: TypeParamWs,
}

#[derive(Debug, Default)]
struct StmtWs {
    idx_by_ids: FxHashMap<Id, usize>,
    ids: FxHashSet<Id>,
    ids_buf: Vec<Id>,
    deps: FxHashSet<Id>,
}

impl Analyzer<'_, '_> {
    /// Returns the order of evaluation. This methods is used to handle hoisting
    /// properly.
    ///
    /// # Exmaple
    ///
    /// The method will return `[1, 0]` for the code below.
    ///
    /// ```js
    /// function foo() {
    ///     return bar();
    /// }
    ///
    /// function bar (){
    ///     return 1;
    /// }K
    /// ```
    pub(super) fn reorder_stmts<T>(&mut self, nodes: &[T]) -> Vec<usize>
    where
        T: StmtLike + for<'any> VisitWith<StmtDependencyFinder<'any>>,
    {
        if nodes.len() <= 1 {
            return (0..nodes.len()).collect();
        }

        let mut ids_graph = DiGraph::<_, usize>::with_capacity(nodes.len(), nodes.len() * 2);

        let order_idx_by_id = &mut self.hoisting_ws.stmts.idx_by_ids;
        let ids = &mut self.hoisting_ws.stmts.ids;
        let ids_buf = &mut self.hoisting_ws.stmts.ids_buf;
        let deps = &mut self.hoisting_ws.stmts.deps;
        let mut graph_node_id_by_id = FxHashMap::<_, _>::default();
        let mut node_ids_by_order_idx = FxHashMap::<_, Vec<_>>::default();

        order_idx_by_id.clear();

        for (idx, node) in nodes.iter().enumerate() {
            ids.clear();
            ids_buf.clear();
            deps.clear();

            {
                let mut v = StmtDependencyFinder { ids_buf, ids, deps };

                node.visit_with(&mut v);
            }

            log::info!("Id graph: ({}) ({:?}) <-- {:?}", idx, ids, deps);
            order_idx_by_id.extend(ids.iter().cloned().map(|id| (id, idx)));

            for (id, is_decl) in ids
                .drain()
                .map(|v| (v, true))
                .chain(deps.drain().map(|v| (v, false)))
            {
                log::info!("Id graph: ({}) ({:?})", idx, id);

                if let Some(node_id) = graph_node_id_by_id.get(&id) {
                    if is_decl {
                        node_ids_by_order_idx.entry(idx).or_default().push(*node_id);
                    }
                    continue;
                }

                let node_id = ids_graph.add_node(id.clone());
                if is_decl {
                    node_ids_by_order_idx.entry(idx).or_default().push(node_id);
                }
                graph_node_id_by_id.insert(id, node_id);
            }
        }

        let mut order = (0..nodes.len()).collect();

        for (i, _) in nodes.iter().enumerate() {
            if let Some(node_ids) = node_ids_by_order_idx.get(&i) {
                for &node_id in node_ids {
                    let mut visitor = DfsPostOrder::new(&ids_graph, node_id);

                    while let Some(node_id) = visitor.next(&ids_graph) {
                        let id = ids_graph.node_weight(node_id).unwrap();
                        let order_of_the_id = order_idx_by_id.get(&id).unwrap();

                        log::error!("Order graph: {} <- {}", i, order_of_the_id);
                    }
                }
            }
        }

        order
    }
}

#[derive(Debug)]
pub(super) struct StmtDependencyFinder<'a> {
    ids_buf: &'a mut Vec<Id>,
#[derive(Debug)]
pub(super) struct StmtDependencyFinder<'a> {
    /// Identifiers created by a statement.
    ///
    /// e.g.
    ///
    /// Value is `[a, b]` for the var declaration below.
    /// ```js
    /// var a, b = foo();
    /// ```
    ids: &'a mut Vec<Id>,

    /// Dependencies of the id.
    dependencies: &'a mut Vec<Id>,
}

impl Visit<FnDecl> for StmtDependencyFinder<'_> {
    fn visit(&mut self, node: &FnDecl) {
        self.ids.push(node.ident.to_id());
        node.visit_children(self);
    }
}

impl Visit<VarDeclarator> for StmtDependencyFinder<'_> {
    fn visit(&mut self, node: &VarDeclarator) {
        let mut v = DestructuringFinder { found: self.ids };
        node.name.visit_with(&mut v);

        node.init.visit_with(self);
    }
}

impl Visit<ClassDecl> for StmtDependencyFinder<'_> {
    fn visit(&mut self, node: &ClassDecl) {
        self.ids.push(node.ident.to_id());
        node.visit_children(self);
    }
}

impl Visit<Function> for StmtDependencyFinder<'_> {
    fn visit(&mut self, f: &Function) {
        let ids = take(self.ids);

        f.visit_children(self);

        *self.ids = ids;
    }
}

impl Visit<MemberExpr> for StmtDependencyFinder<'_> {
    fn visit(&mut self, node: &MemberExpr) {
        node.obj.visit_with(self);

        if node.computed {
            node.prop.visit_with(self);
        }
    }
}

impl Visit<Expr> for StmtDependencyFinder<'_> {
    fn visit(&mut self, node: &Expr) {
        match node {
            Expr::Ident(ref i) => {
                self.dependencies.push(i.to_id());
            }
            _ => {}
        }

        node.visit_children(self);
    }
}

pub(super) fn order_type_params(params: &[TsTypeParam]) -> ValidationResult<Vec<usize>> {
    let mut graph = DiGraphMap::<usize, usize>::with_capacity(params.len(), params.len() * 2);
    let mut deps = FxHashSet::with_capacity_and_hasher(4, Default::default());
    for i in 0..params.len() {
        graph.add_node(i);
    }

    for (idx, node) in params.iter().enumerate() {
        deps.clear();

        {
            let mut v = TypeParamDepFinder {
                id: &node.name.sym,
                deps: &mut deps,
            };

            for p in params {
                p.constraint.visit_with(&mut v);
                p.default.visit_with(&mut v);
            }

            for dep in &deps {
                if let Some(pos) = params.iter().position(|v| v.name.sym == *dep) {
                    //
                    graph.add_edge(idx, pos, 1);
                    break;
                }
            }

            log::info!("Reorder: {} <-- {:?}", node.name.sym, deps);
        }
    }

    let sorted = toposort(&graph.into_graph::<usize>(), None)
        .expect("cycle in type parameter is not supported");
    log::warn!("Reorder: {:?}", sorted);
    Ok(sorted.into_iter().map(|i| i.index()).collect())
}

#[derive(Debug)]
struct TypeParamDepFinder<'a> {
    id: &'a JsWord,
    deps: &'a mut FxHashSet<JsWord>,
}

impl Visit<Ident> for TypeParamDepFinder<'_> {
    fn visit(&mut self, node: &Ident) {
        if *self.id == node.sym {
            return;
        }

        self.deps.insert(node.sym.clone());
    }
}
