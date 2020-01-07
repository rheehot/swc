pub(crate) use self::scope::ScopeKind;
use self::{control_flow::CondFacts, scope::Scope, util::ResultExt};
use crate::{
    analyzer::{pat::PatMode, props::ComputedPropMode},
    errors::Error,
    loader::Load,
    ty,
    ty::{Alias, Type},
    validator::{Validate, ValidateWith},
    Exports, ImportInfo, Rule,
};
use fxhash::{FxHashMap, FxHashSet};
use std::{
    ops::{Deref, DerefMut},
    path::PathBuf,
    sync::Arc,
};
use swc_atoms::JsWord;
use swc_common::Span;
use swc_common::{SourceMap, Span};
use swc_common::{Span, Visit};
use swc_common::{Span, Spanned, Visit, VisitWith};
use swc_ecma_ast::*;
use swc_ts_builtin_types::Lib;

#[macro_use]
mod macros;
mod assign;
mod class;
mod control_flow;
mod convert;
mod enums;
mod export;
mod expr;
mod function;
mod generic;
mod import;
mod name;
mod pat;
mod props;
mod scope;
mod stmt;
mod type_facts;
mod type_params;
mod util;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Ctx {
    in_declare: bool,
    pat_mode: PatMode,
    computed_prop_mode: ComputedPropMode,
    allow_ref_declaring: bool,
}

/// Note: All methods named `validate_*` return [Err] iff it's not recoverable.
#[derive(Debug)]
pub(crate) struct Analyzer<'a> {
pub struct Analyzer<'a,'b> {
pub struct Analyzer<'a, 'b> {
    pub info: Info,

    resolved_imports: FxHashMap<JsWord, Type>,
    errored_imports: FxHashSet<JsWord>,
    pending_exports: Vec<((JsWord, Span), Expr)>,

    rule: Rule,
    libs: &'b [Lib],
    scope: Scope<'a>,

    ctx: Ctx,

    loader: &'b dyn Load,

    is_builtin: bool,
}

#[derive(Debug, Clone, Default)]
pub struct Info {
    pub errors: Vec<Error>,
    pub exports: Exports<FxHashMap<JsWord, Type>>,
}

impl Analyzer<'_> {
fn _assert_types() {
    fn is_sync<T: Sync>() {}
    fn is_send<T: Send>() {}
    is_sync::<Info>();
    is_send::<Info>();
}

impl<'a, 'b> Analyzer<'a, 'b> {
    pub fn root(libs: &'b [Lib], rule: Rule, loader: &'b dyn Load) -> Self {
        Self::new_inner(libs, rule, loader, Scope::root(), false)
    }

    pub(crate) fn for_builtin() -> Self {
        Self::new_inner(&[], Default::default(), &NoopLoader, Scope::root(), true)
    }

    fn new(&self, scope: Scope<'a>) -> Self {
        Self::new_inner(self.libs, self.rule, self.loader, scope, false)
    }

    fn new_inner(
        libs: &'b [Lib],
        rule: Rule,
        loader: &'b dyn Load,
        scope: Scope<'a>,
        is_builtin: bool,
    ) -> Self {
        Self {
            info: Default::default(),
            resolved_imports: Default::default(),
            errored_imports: Default::default(),
            pending_exports: Default::default(),
            rule,
            libs,
            scope,
            ctx: Ctx {
                in_declare: false,
                allow_ref_declaring: false,
                pat_mode: PatMode::Assign,
                computed_prop_mode: ComputedPropMode::Object,
            },
            loader,
            is_builtin,
        }
    }

    fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> Ret
    pub(super) fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> Ret
    where
        F: for<'any> FnOnce(&mut Analyzer<'any>) -> Ret,
        where
            F: for<'aa,'bb> FnOnce(&mut Analyzer<'aa,'bb>) -> Ret,
    #[inline(always)]
    pub(crate) fn with<F, Ret>(&mut self, op: F) -> Ret
    where
        F: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>) -> Ret,
    {
        op(self)
    }

    pub(crate) fn with_child<F, Ret>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> Ret
    where
        F: for<'aa, 'bb> FnOnce(&mut Analyzer<'aa, 'bb>) -> Ret,
    {
        let ctx = self.ctx;
        let child_scope = Scope::new(&self.scope, kind, facts);
        let (ret, info) = {
            let mut child = self.new(child_scope);
            child.ctx = ctx;

            let ret = op(&mut child);

            (ret, child.info)
        };

        self.info.errors.extend(info.errors);
        assert!(info.exports.types.is_empty(), "child cannot export a type");
        assert!(
            info.exports.vars.is_empty(),
            "child cannot export a variable"
        );

        ret
    }

    pub(super) fn with_ctx(&mut self, ctx: Ctx) -> WithCtx<'_, 'a, 'b> {
        let orig_ctx = self.ctx;
        self.ctx = ctx;
        WithCtx {
            analyzer: self,
            orig_ctx,
        }
    }
}

pub(super) struct WithCtx<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
    orig_ctx: Ctx,
}

impl Drop for WithCtx<'_, '_, '_> {
    fn drop(&mut self) {
        self.analyzer.ctx = self.orig_ctx;
    }
}

impl<'b, 'c> Deref for WithCtx<'_, 'b, 'c> {
    type Target = Analyzer<'b, 'c>;

    fn deref(&self) -> &Self::Target {
        &self.analyzer
    }
}

impl<'b, 'c> DerefMut for WithCtx<'_, 'b, 'c> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.analyzer
    }
}

struct NoopLoader;
impl Load for NoopLoader {
    fn load(
        &self,
        _: Arc<PathBuf>,
        _: &ImportInfo,
    ) -> Result<Exports<FxHashMap<JsWord, Type>>, Error> {
        unreachable!("builtin module should not import other module")
    }
}

/// Done
impl Visit<Decorator> for Analyzer<'_, '_> {
    fn visit(&mut self, d: &Decorator) {
        self.validate(&d.expr).store(&mut self.info.errors);
    }
}

impl Visit<TsImportEqualsDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &TsImportEqualsDecl) {
        match node.module_ref {
            TsModuleRef::TsEntityName(ref e) => {
                match self.type_of_ts_entity_name(node.span, e, None) {
                    Ok(..) => {}
                    Err(err) => self.info.errors.push(err),
                }
            }
            _ => {}
        }
    }
}

impl Visit<TsModuleDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, decl: &TsModuleDecl) {
        let span = decl.span;

        let mut new = self.new(Scope::root());
        new.ctx.in_declare = decl.declare;

        decl.visit_children(&mut new);
        self.info.errors.append(&mut new.info.errors);

        self.register_type(
            match decl.id {
                TsModuleName::Ident(ref i) => i.sym.clone(),
                TsModuleName::Str(ref s) => s.value.clone(),
            },
            Type::Module(ty::Module {
                span,
                exports: new.info.exports,
            }),
        )
        .store(&mut self.info.errors);
    }
}
