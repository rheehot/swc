//! Utilities for testing.
use super::Bundler;
use crate::{util::HygieneRemover, Load, Resolve};
use std::path::PathBuf;
use swc_common::{FileName, GLOBALS};
use swc_ecma_ast::*;
use swc_ecma_parser::{EsConfig, Syntax};
use swc_ecma_utils::drop_span;
use swc_ecma_visit::FoldWith;

pub struct Tester<'a> {
    pub bundler: Bundler<'a, Loader, Resolver>,
}

#[derive(Debug, Default)]
struct Loader;

impl Load for Loader {
    fn load(
        &self,
        _: &FileName,
    ) -> Result<(std::sync::Arc<swc_common::SourceFile>, Module), anyhow::Error> {
        unreachable!("swc_bundler: tester.load")
    }
}

#[derive(Debug, Default)]
struct Resolver;

impl Resolve for Resolver {
    fn resolve(&self, base: &FileName, module_specifier: &str) -> Result<FileName, anyhow::Error> {
        unreachable!("swc_bundler: tester.resolve")
    }
}

impl<'a> Tester<'a> {
    pub fn parse(&self, s: &str) -> Module {
        let fm = self
            .bundler
            .swc
            .cm
            .new_source_file(FileName::Real(PathBuf::from("input.js")), s.into());
        let p = self
            .bundler
            .swc
            .parse_js(
                fm,
                Default::default(),
                Syntax::Es(EsConfig {
                    dynamic_import: true,
                    ..Default::default()
                }),
                true,
                true,
            )
            .expect("failed to parse");

        match p {
            Program::Module(m) => m,
            Program::Script(_) => unreachable!(),
        }
    }

    pub fn assert_eq(&self, m: &Module, expected: &str) {
        let expected = self.parse(expected);

        let m = drop_span(m.clone().fold_with(&mut HygieneRemover));
        let expected = drop_span(expected);

        assert_eq!(m, expected)
    }
}

pub fn test_bundler<F>(op: F)
where
    F: FnOnce(&mut Tester),
{
    testing::run_test2(true, |cm, handler| {
        GLOBALS.with(|globals| {
            let bundler = Bundler::new(globals, Default::default(), Default::default(), vec![]);

            let mut t = Tester { bundler };

            op(&mut t);

            Ok(())
        })
    })
    .expect("WTF?");
}
