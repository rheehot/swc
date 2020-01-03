use self::scope::Scope;
use crate::{errors::Error, ty::TypeRef, Exports};
use fxhash::FxHashMap;
use std::sync::Arc;
use swc_atoms::JsWord;

mod control_flow;
mod expr;
mod name;
mod scope;
mod stmt;
mod util;

/// Note: All methods named `validate_*` return [Err] iff it's not recoverable.
#[derive(Debug)]
pub struct Analyzer<'a> {
    pub info: Info,

    scope: Scope<'a>,
}

#[derive(Debug, Default)]
pub struct Info {
    pub errors: Vec<Error>,
    pub exports: Exports<FxHashMap<JsWord, Arc<TypeRef<'static>>>>,
}
