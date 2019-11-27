#![feature(box_syntax)]
#![feature(specialization)]

extern crate lazy_static;
extern crate swc_atoms;
extern crate swc_common;
extern crate swc_ecma_ast;
extern crate swc_ecma_parser;
extern crate swc_ts_builtin_macro;

use lazy_static::lazy_static;
use std::sync::Arc;
use swc_atoms::js_word;
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, FilePathMapping, Fold, FoldWith, SourceMap, Span, DUMMY_SP,
};
use swc_ecma_ast::*;
use swc_ecma_parser::{Parser, Session, SourceFileInput, Syntax};
use swc_ts_builtin_macro::builtin;

// macro_rules! lib {
//     (
//         Names {
//             $($name:ident: $s:expr,)*
//         }
//     ) => {
//         #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//         pub enum Lib {
//             $(
//                 $name,
//             )*
//         }

//         $(
//             builtin!($name, $s);
//         )*

//         impl Lib {
//             fn body(self) -> &'static TsNamespaceDecl {
//                 match self {
//                     $(
//                         Lib::$name => &*$name,
//                     )*
//                 }
//             }
//         }
//     };
// }

builtin!();

/// Merge definitions
pub fn load(libs: &[Lib]) -> Vec<&'static TsNamespaceDecl> {
    libs.iter().map(|lib| lib.body()).collect()
}
