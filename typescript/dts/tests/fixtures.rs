#![recursion_limit = "256"]
#![feature(vec_remove_item)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]
#![feature(test)]

extern crate test;

use anyhow::{Context, Error};
use std::{
    collections::HashSet,
    env,
    fs::File,
    io::{self, Read},
    path::Path,
    process::Command,
    sync::Arc,
};
use swc_common::{
    comments::Comments, errors::DiagnosticBuilder, FileName, Fold, FoldWith, Span, Spanned,
};
use swc_ecma_ast::{Module, *};
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};
use swc_ecma_parser::{JscTarget, Parser, Session, SourceFileInput, Syntax, TsConfig};
use swc_ts_checker::{Lib, Rule};
use swc_ts_dts::generate_dts;
use test::{test_main, DynTestFn, ShouldPanic::No, TestDesc, TestDescAndFn, TestName, TestType};
use testing::{StdErr, Tester};

#[test]
fn fixtures() {
    let args: Vec<_> = env::args().collect();
    let mut tests = Vec::new();
    add_tests(&mut tests).unwrap();
    test_main(&args, tests, Default::default());
}

fn add_tests(tests: &mut Vec<TestDescAndFn>) -> Result<(), Error> {
    let root = {
        let mut root = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
        root.push("tests");
        root.push("fixtures");

        root
    };

    for entry in ignore::WalkBuilder::new(&root)
        .ignore(false)
        .git_exclude(false)
        .git_global(false)
        .git_ignore(false)
        .hidden(true)
        .build()
    {
        let entry = entry.context("entry?")?;
        match entry.path().file_stem() {
            Some(ext) if ext.to_string_lossy() == "index" => {}
            _ => {
                continue;
            }
        }

        let is_ts = entry.file_name().to_string_lossy().ends_with(".ts")
            || entry.file_name().to_string_lossy().ends_with(".tsx");
        if entry.file_type().unwrap().is_dir() || !is_ts {
            continue;
        }

        let file_name = entry
            .path()
            .strip_prefix(&root)
            .expect("failed to strip prefix")
            .to_str()
            .unwrap()
            .to_string();

        let input = {
            let mut buf = String::new();
            File::open(entry.path())?.read_to_string(&mut buf)?;
            buf
        };

        let test_name = file_name.replace("/", "::");
        let ignore = false;

        let name = test_name.to_string();
        add_test(tests, name, ignore, move || {
            do_test(entry.path()).unwrap();
        });
    }

    Ok(())
}

fn do_test(file_name: &Path) -> Result<(), StdErr> {
    let fname = file_name.display().to_string();

    testing::Tester::new()
        .print_errors(|cm, handler| {
            let handler = Arc::new(handler);

            let checker = swc_ts_checker::Checker::new(
                Default::default(),
                cm.clone(),
                handler.clone(),
                Default::default(),
                Default::default(),
                TsConfig {
                    tsx: fname.contains("tsx"),
                    ..Default::default()
                },
                JscTarget::Es5,
            );

            let info = checker.check(Arc::new(file_name.into()));
            let errors = ::swc_ts_checker::errors::Error::flatten(info.1.errors.into());

            let has_errors = !errors.is_empty();
            checker.run(|| {
                for e in errors {
                    e.emit(&handler);
                }
            });

            if has_errors {
                return Err(());
            }

            let dts = generate_dts(info.0, info.1.exports);

            let generated = {
                let mut buf = vec![];
                {
                    let handlers = box MyHandlers;
                    let mut emitter = Emitter {
                        cfg: Default::default(),
                        comments: None,
                        cm: cm.clone(),
                        wr: box JsWriter::new(cm.clone(), "\n", &mut buf, None),
                        handlers,
                    };

                    emitter
                        .emit_module(&dts)
                        .context("failed to emit module")
                        .unwrap();
                }
                String::from_utf8(buf).unwrap()
            };

            StdErr::from(generated)
                .compare_to_file(file_name.parent().unwrap().join("index.d.ts"))
                .unwrap();

            Ok(())
        })
        .expect("failed to check");

    Ok(())
}

fn get_correct_dts(path: &Path) -> Module {
    testing::run_test2(false, |cm, handler| {
        let mut c = Command::new("tsc")
            .arg(path)
            .arg("-d")
            .arg("--emitDeclarationOnly")
            .output()
            .unwrap();

        Ok(())
    })
    .unwrap()
}

fn add_test<F: FnOnce() + Send + 'static>(
    tests: &mut Vec<TestDescAndFn>,
    name: String,
    ignore: bool,
    f: F,
) {
    tests.push(TestDescAndFn {
        desc: TestDesc {
            test_type: TestType::UnitTest,
            name: TestName::DynTestName(name),
            ignore,
            should_panic: No,
            allow_fail: false,
        },
        testfn: DynTestFn(box f),
    });
}

struct MyHandlers;

impl swc_ecma_codegen::Handlers for MyHandlers {}
