#![recursion_limit = "256"]
#![feature(vec_remove_item)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]
#![feature(test)]

extern crate test;

use anyhow::Error;
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
        let entry = entry?;
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
            JscTarget::Es5k,
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

fn make_test(c: &Comments, module: Module) -> Module {
    let mut m = TestMaker {
        c,
        stmts: Default::default(),
    };

    module.fold_with(&mut m)
}

struct TestMaker<'a> {
    c: &'a Comments,
    stmts: Vec<Stmt>,
}

impl Fold<Vec<ModuleItem>> for TestMaker<'_> {
    fn fold(&mut self, stmts: Vec<ModuleItem>) -> Vec<ModuleItem> {
        let mut ss = vec![];
        for stmt in stmts {
            let stmt = stmt.fold_with(self);
            ss.push(stmt);
            ss.extend(self.stmts.drain(..).map(ModuleItem::Stmt));
        }

        ss
    }
}

impl Fold<Vec<Stmt>> for TestMaker<'_> {
    fn fold(&mut self, stmts: Vec<Stmt>) -> Vec<Stmt> {
        let mut ss = vec![];
        for stmt in stmts {
            let stmt = stmt.fold_with(self);
            ss.push(stmt);
            ss.extend(self.stmts.drain(..));
        }

        ss
    }
}

impl Fold<TsTypeAliasDecl> for TestMaker<'_> {
    fn fold(&mut self, decl: TsTypeAliasDecl) -> TsTypeAliasDecl {
        let cmts = self.c.trailing_comments(decl.span.hi());

        match cmts {
            Some(cmts) => {
                assert!(cmts.len() == 1);
                let cmt = cmts.iter().next().unwrap();
                let t = cmt.text.trim().replace("\n", "").replace("\r", "");

                let cmt_type = match parse_type(cmt.span, &t) {
                    Some(ty) => ty,
                    None => return decl,
                };

                //  {
                //      let _value: ty = (Object as any as Alias)
                //  }
                //
                //
                let span = decl.span();
                self.stmts.push(Stmt::Block(BlockStmt {
                    span,
                    stmts: vec![Stmt::Decl(Decl::Var(VarDecl {
                        span,
                        decls: vec![VarDeclarator {
                            span,
                            name: Pat::Ident(Ident {
                                span,
                                sym: "_value".into(),
                                type_ann: Some(TsTypeAnn {
                                    span,
                                    type_ann: box cmt_type,
                                }),
                                optional: false,
                            }),
                            init: Some(box Expr::TsAs(TsAsExpr {
                                span,
                                expr: box Expr::TsAs(TsAsExpr {
                                    span,
                                    expr: box Expr::Ident(Ident::new("Object".into(), span)),
                                    type_ann: box TsType::TsKeywordType(TsKeywordType {
                                        span,
                                        kind: TsKeywordTypeKind::TsAnyKeyword,
                                    }),
                                }),
                                type_ann: box TsType::TsTypeRef(TsTypeRef {
                                    span,
                                    type_name: TsEntityName::Ident(decl.id.clone()),
                                    type_params: None,
                                }),
                            })),
                            definite: false,
                        }],
                        kind: VarDeclKind::Const,
                        declare: false,
                    }))],
                }));
            }
            None => {}
        }

        decl
    }
}

fn parse_type(span: Span, s: &str) -> Option<TsType> {
    let s = s.trim();

    if s.starts_with("error") || s.starts_with("Error") {
        return None;
    }

    let ty = ::testing::run_test(true, |cm, handler| {
        let session = Session { handler: &handler };

        let fm = cm.new_source_file(FileName::Anon, s.into());

        let mut parser = Parser::new(
            session,
            Syntax::Typescript(Default::default()),
            SourceFileInput::from(&*fm),
            None,
        );
        let ty = match parser.parse_type() {
            Ok(v) => v,
            Err(..) => return Err(()),
        };
        Ok(ty)
    });

    let mut spanner = Spanner { span };

    Some(*ty.ok()?.fold_with(&mut spanner))
}

struct Spanner {
    span: Span,
}

impl Fold<Span> for Spanner {
    fn fold(&mut self, _: Span) -> Span {
        self.span
    }
}
