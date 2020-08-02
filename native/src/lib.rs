#![recursion_limit = "2048"]

#[macro_use]
extern crate napi;
#[macro_use]
extern crate napi_derive;

use anyhow::Error;
use backtrace::Backtrace;
use napi::{Module, Result};
use std::{env, panic::set_hook, sync::Arc};
use swc::{Compiler, TransformOutput};
use swc_common::{self, errors::Handler, FilePathMapping, SourceMap};

mod bundle;
mod parse;
mod print;
mod transform;
mod util;

register_module!(native, init_module);

fn init_module(module: &mut Module) -> Result<()> {
    module.create_named_method("transform", transform::transform)?;
    module.create_named_method("transformSync", transform::transform_sync)?;
    Ok(())
}

// fn init(_cx: MethodContext<JsUndefined>) -> NeonResult<ArcCompiler> {
//     if cfg!(debug_assertions) || env::var("SWC_DEBUG").unwrap_or_else(|_|
// String::new()) == "1" {         set_hook(Box::new(|_panic_info| {
//             let backtrace = Backtrace::new();
//             println!("Backtrace: {:?}", backtrace);
//         }));
//     }
//
//     let cm = Arc::new(SourceMap::new(FilePathMapping::empty()));
//
//     let handler = Arc::new(Handler::with_tty_emitter(
//         swc_common::errors::ColorConfig::Always,
//         true,
//         false,
//         Some(cm.clone()),
//     ));
//
//     let c = Compiler::new(cm.clone(), handler);
//
//     Ok(Arc::new(c))
// }
//
// pub fn complete_output<'a>(
//     mut cx: impl Context<'a>,
//     result: Result<TransformOutput, Error>,
// ) -> JsResult<'a, JsValue> {
//     match result {
//         Ok(output) => Ok(neon_serde::to_value(&mut cx, &output)?),
//         Err(err) => cx.throw_error(format!("{:?}", err)),
//     }
// }
//
// pub type ArcCompiler = Arc<Compiler>;
//
// declare_types! {
//     pub class JsCompiler for ArcCompiler {
//         init(cx) {
//             init(cx)
//         }
//
//         method transform(cx) {
//             transform::transform(cx)
//         }
//
//         method transformSync(cx) {
//             transform::transform_sync(cx)
//         }
//
//         method transformFile(cx) {
//             transform::transform_file(cx)
//         }
//
//         method transformFileSync(cx) {
//             transform::transform_file_sync(cx)
//         }
//
//         method parse(cx) {
//             parse::parse(cx)
//         }
//
//         method parseSync(cx) {
//             parse::parse_sync(cx)
//         }
//
//         method parseFile(cx) {
//             parse::parse_file(cx)
//         }
//
//         method parseFileSync(cx) {
//             parse::parse_file_sync(cx)
//         }
//
//         method print(cx) {
//             print::print(cx)
//         }
//
//         method printSync(cx) {
//             print::print_sync(cx)
//         }
//
//         method bundle(cx) {
//             bundle::bundle(cx)
//         }
//     }
// }
//
// register_module!(mut cx, {
//     cx.export_class::<JsCompiler>("Compiler")?;
//     Ok(())
// });
