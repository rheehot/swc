use crate::util::ErrorExt;
use anyhow::{Context as _, Context, Error};
use napi::{CallContext, Env, JsBoolean, JsObject, JsString, JsUndefined, Result, Status, Task};
use path_clean::clean;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use swc::{config::Options, Compiler, TransformOutput};
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, FilePathMapping, SourceFile, SourceMap,
};
use swc_ecma_ast::Program;

/// Input to transform
#[derive(Debug)]
pub enum Input {
    /// json string
    Program(String),
    /// Raw source code.
    Source(Arc<SourceFile>),
    /// File
    File(PathBuf),
}

pub struct TransformTask {
    pub c: Arc<Compiler>,
    pub input: Input,
    pub options: Options,
}

impl Task for TransformTask {
    type Output = TransformOutput;
    type JsValue = JsObject;

    fn compute(&mut self) -> Result<Self::Output> {
        self.c.run(|| match self.input {
            Input::Program(ref s) => {
                let program: Program = serde_json::from_str(&s)
                    .context("failed to deserialize Program")
                    .convert_err(Status::InvalidArg)?; // TODO: Source map

                self.c
                    .process_js(program, &self.options)
                    .convert_err(Status::GenericFailure)
            }

            Input::File(ref path) => {
                let fm = self
                    .c
                    .cm
                    .load_file(path)
                    .context("failed to read module")
                    .convert_err(Status::InvalidArg)?;
                self.c
                    .process_js_file(fm, &self.options)
                    .convert_err(Status::GenericFailure)
            }

            Input::Source(ref s) => self
                .c
                .process_js_file(s.clone(), &self.options)
                .convert_err(Status::GenericFailure),
        })
    }

    fn resolve(&self, env: &mut Env, output: Self::Output) -> Result<Self::JsValue> {
        let mut obj = env.create_object()?;
        obj.set_named_property("code", env.create_string_from_std(output.code)?)?;
        if let Some(map) = output.map {
            obj.set_named_property("map", env.create_string_from_std(map)?)?;
        }
        Ok(obj)
    }
}

#[js_function(3)]
pub fn transform<'env>(c: CallContext<'env>) -> Result<JsUndefined> {
    c.env.get_undefined()
}

#[js_function(3)]
pub fn transform_sync<'env>(c: CallContext<'env>) -> Result<JsObject> {
    let cm = Arc::new(SourceMap::new(FilePathMapping::empty()));
    let handler = Arc::new(Handler::with_tty_emitter(
        ColorConfig::Always,
        false,
        false,
        Some(cm.clone()),
    ));

    let src = c.get::<JsString>(0)?;
    let is_module = c.get::<JsBoolean>(1)?;
    let options = c.get::<JsObject>(2)?;

    let input = if is_module.get_value()? {
        Input::Program(src.as_str()?.to_string())
    } else {
        Input::Source(cm.new_source_file(
            if let Ok(v) = options.get_named_property::<JsString>("filename") {
                FileName::Real(Path::new(v.as_str()?).to_path_buf())
            } else {
                FileName::Anon
            },
            src.as_str()?.to_string(),
        ))
    };

    let mut task = TransformTask {
        c: Arc::new(Compiler::new(cm.clone(), handler)),
        input,
        options: Default::default(),
    };

    let output = task.compute()?;

    let mut obj = c.env.create_object()?;
    obj.set_named_property("code", c.env.create_string_from_std(output.code)?)?;
    if let Some(map) = output.map {
        obj.set_named_property("map", c.env.create_string_from_std(map)?)?;
    }
    Ok(obj)
}

//
// /// returns `compiler, (src / path), options, plugin, callback`
// pub fn schedule_transform<F>(mut cx: MethodContext<JsCompiler>, op: F) ->
// JsResult<JsValue> where
//     F: FnOnce(&Arc<Compiler>, String, bool, Options) -> TransformTask,
// {
//     let c;
//     let this = cx.this();
//     {
//         let guard = cx.lock();
//         c = this.borrow(&guard).clone();
//     };
//
//     let s = cx.argument::<JsString>(0)?.value();
//     let is_module = cx.argument::<JsBoolean>(1)?;
//     let options_arg = cx.argument::<JsValue>(2)?;
//
//     let options: Options = neon_serde::from_value(&mut cx, options_arg)?;
//     let callback = cx.argument::<JsFunction>(3)?;
//
//     let task = op(&c, s, is_module.value(), options);
//     task.schedule(callback);
//
//     Ok(cx.undefined().upcast())
// }
//
// pub fn exec_transform<F>(mut cx: MethodContext<JsCompiler>, op: F) ->
// JsResult<JsValue> where
//     F: FnOnce(&Compiler, String, &Options) -> Result<Arc<SourceFile>, Error>,
// {
//     let s = cx.argument::<JsString>(0)?;
//     let is_module = cx.argument::<JsBoolean>(1)?;
//     let options: Options = match cx.argument_opt(2) {
//         Some(v) => neon_serde::from_value(&mut cx, v)?,
//         None => {
//             let obj = cx.empty_object().upcast();
//             neon_serde::from_value(&mut cx, obj)?
//         }
//     };
//
//     let this = cx.this();
//     let output = {
//         let guard = cx.lock();
//         let c = this.borrow(&guard);
//         c.run(|| {
//             if is_module.value() {
//                 let program: Program =
//                     serde_json::from_str(&s.value()).expect("failed to
// deserialize Program");                 c.process_js(program, &options)
//             } else {
//                 let fm = op(&c, s.value(), &options).expect("failed to create
// fm");                 c.process_js_file(fm, &options)
//             }
//         })
//     };
//
//     complete_output(cx, output)
// }
// pub fn transform_sync(cx: MethodContext<JsCompiler>) -> JsResult<JsValue> {
//     exec_transform(cx, |c, src, options| {
//         Ok(c.cm.new_source_file(
//             if options.filename.is_empty() {
//                 FileName::Anon
//             } else {
//                 FileName::Real(options.filename.clone().into())
//             },
//             src,
//         ))
//     })
// }
//
// pub fn transform_file(cx: MethodContext<JsCompiler>) -> JsResult<JsValue> {
//     schedule_transform(cx, |c, path, _, options| {
//         let path = clean(&path);
//
//         TransformTask {
//             c: c.clone(),
//             input: Input::File(path.into()),
//             options,
//         }
//     })
// }
//
// pub fn transform_file_sync(cx: MethodContext<JsCompiler>) ->
// JsResult<JsValue> {     exec_transform(cx, |c, path, _| {
//         Ok(c.cm
//             .load_file(Path::new(&path))
//             .expect("failed to load file"))
//     })
// }
