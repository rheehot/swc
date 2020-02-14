#![feature(box_syntax)]
#![feature(specialization)]

use dashmap::DashMap;
use once_cell::sync::Lazy;
use std::sync::Arc;
use swc_common::{
    errors::{ColorConfig, Handler},
    fold::FoldWith,
    input::SourceFileInput,
    FileName, FilePathMapping, Fold, SourceMap, Span, DUMMY_SP,
};
use swc_ecma_ast::*;
use swc_ecma_parser::{Parser, Session, Syntax};
use swc_ts_builtin_macro::builtin;

builtin!();

impl Lib {
    fn body(self) -> &'static TsNamespaceDecl {
        static CACHE: Lazy<DashMap<Lib, &'static TsNamespaceDecl>> = Lazy::new(Default::default);

        if let Some(v) = CACHE.get(&self) {
            return &**v.value();
        }

        let ns = Box::leak(box parse(self.content()));

        CACHE.insert(self, &*ns);

        &*ns
    }
}

/// Merge definitions
pub fn load(libs: &[Lib]) -> Vec<&'static TsNamespaceDecl> {
    libs.iter().map(|lib| lib.body()).collect()
}

fn parse(content: &str) -> TsNamespaceDecl {
    let cm = Arc::new(SourceMap::new(FilePathMapping::empty()));
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));

    let fm = cm.new_source_file(FileName::Anon, content.to_string());

    let session = Session { handler: &handler };

    let mut parser = Parser::new(
        session,
        Syntax::Typescript(Default::default()),
        SourceFileInput::from(&*fm),
        None,
    );

    // We cannot use parse_module because of `eval`
    let mut script = parser
        .parse_script()
        .map_err(|mut e| {
            e.emit();
            ()
        })
        .expect("failed to parse module");

    assert_eq!(script.body.len(), 1);
    match script.body.pop().unwrap() {
        Stmt::Decl(Decl::TsModule(m)) => match m.body.unwrap() {
            TsNamespaceBody::TsModuleBlock(_) => unreachable!(),
            TsNamespaceBody::TsNamespaceDecl(d) => d.fold_with(&mut DropSpan),
        },
        _ => unreachable!(),
    }
}

#[derive(Clone, Copy)]
struct DropSpan;
impl Fold<Span> for DropSpan {
    fn fold(&mut self, _: Span) -> Span {
        DUMMY_SP
    }
}
