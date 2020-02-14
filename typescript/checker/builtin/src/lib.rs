#![feature(box_syntax)]
#![feature(specialization)]

extern crate lazy_static;
extern crate swc_atoms;
extern crate swc_common;
extern crate swc_ecma_ast;
extern crate swc_ecma_parser;
extern crate swc_ts_builtin_macro;

use lazy_static::lazy_static;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ts_builtin_macro::builtin;

builtin!();

/// Merge definitions
pub fn load(libs: &[Lib]) -> Vec<&'static TsNamespaceDecl> {
    //    libs.iter().map(|lib| lib.body()).collect()
    unimplemented!()
}
fn parse_namespace<T>(n: T) {}
