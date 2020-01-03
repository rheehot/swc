use self::scope::Scope;
use crate::{errors::Error, ty::TypeRef, Exports};
use fxhash::FxHashMap;
use std::sync::Arc;
use swc_atoms::JsWord;

mod expr;
mod scope;
mod stmt;
mod util;

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
