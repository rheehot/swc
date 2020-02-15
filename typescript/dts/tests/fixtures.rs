#![recursion_limit = "256"]
#![feature(vec_remove_item)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]
#![feature(test)]

extern crate test;

use anyhow::{Error, Context};
use std::{
    collections::HashSet,
    env,
    fs::File,
    io::{self, Read},
    path::Path,
    sync::Arc,
};
use swc_common::{
    comments::Comments, errors::DiagnosticBuilder, FileName, Fold, FoldWith, Span, Spanned,
};
use swc_ecma_ast::{Module, *};
use swc_ecma_parser::{JscTarget, Parser, Session, SourceFileInput, Syntax, TsConfig};
use swc_ts_checker::{Lib, Rule};
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

    ::testing::Tester::new().print_errors(|cm, handler| {
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

        let errors =
            ::swc_ts_checker::errors::Error::flatten(checker.check(Arc::new(file_name.into())));

        let has_errors = !errors.is_empty();
        checker.run(|| {
            for e in errors {
                e.emit(&handler);
            }
        });

        if has_errors {
            Err(())
        } else {
            Ok(())
        }
    });

    Ok(())
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
