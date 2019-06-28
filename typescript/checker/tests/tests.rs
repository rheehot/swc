#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(specialization)]
#![feature(test)]

extern crate swc_common;
extern crate swc_ecma_ast;
extern crate swc_ecma_parser;
extern crate swc_ts_checker;
extern crate test;
extern crate testing;
extern crate walkdir;

use std::{
    env,
    fs::File,
    io::{self, Read},
    path::Path,
};
use swc_common::{comments::Comments, Fold, FoldWith, Span, Spanned, CM};
use swc_ecma_ast::{Module, *};
use swc_ecma_parser::{Parser, Session, SourceFileInput, Syntax, TsConfig};
use swc_ts_checker::{Lib, Rule};
use test::{test_main, DynTestFn, Options, ShouldPanic::No, TestDesc, TestDescAndFn, TestName};
use testing::StdErr;
use walkdir::WalkDir;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Error,
    Pass,
    Conformance,
}

#[test]
fn conformance() {
    let args: Vec<_> = env::args().collect();
    let mut tests = Vec::new();
    add_tests(&mut tests, Mode::Conformance).unwrap();
    test_main(&args, tests, Options::new());
}

#[test]
fn passes() {
    let args: Vec<_> = env::args().collect();
    let mut tests = Vec::new();
    add_tests(&mut tests, Mode::Pass).unwrap();
    test_main(&args, tests, Options::new());
}

#[test]
fn errors() {
    let args: Vec<_> = env::args().collect();
    let mut tests = Vec::new();
    add_tests(&mut tests, Mode::Error).unwrap();
    test_main(&args, tests, Options::new());
}

fn add_tests(tests: &mut Vec<TestDescAndFn>, mode: Mode) -> Result<(), io::Error> {
    let test_kind = match mode {
        Mode::Error => "errors",
        Mode::Conformance => "conformance",
        Mode::Pass => "pass",
    };

    let root = {
        let mut root = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
        root.push("tests");
        root.push(test_kind);

        root
    };

    eprintln!("Loading tests from {}", root.display());

    let dir = root;

    for entry in WalkDir::new(&dir).into_iter() {
        let entry = entry?;
        println!("{}", entry.file_name().to_string_lossy());
        let is_ts = entry.file_name().to_string_lossy().ends_with(".ts")
            || entry.file_name().to_string_lossy().ends_with(".tsx");
        if entry.file_type().is_dir() || !is_ts {
            continue;
        }

        let is_not_index = !entry.file_name().to_string_lossy().ends_with("index.d.ts")
            && !entry.file_name().to_string_lossy().ends_with("index.ts")
            && !entry.file_name().to_string_lossy().ends_with("index.tsx");
        if is_not_index && mode != Mode::Conformance {
            continue;
        }

        let file_name = entry
            .path()
            .strip_prefix(&dir)
            .expect("failed to strip prefix")
            .to_str()
            .unwrap()
            .to_string();

        let input = {
            let mut buf = String::new();
            File::open(entry.path())?.read_to_string(&mut buf)?;
            buf
        };

        let ignore = file_name.contains("circular")
            || input.contains("@filename")
            || (mode == Mode::Conformance && !file_name.contains("types/unknown"));

        let dir = dir.clone();
        let name = format!("tsc::{}::{}", test_kind, file_name);
        add_test(tests, name, ignore, move || {
            if mode == Mode::Error || mode == Mode::Conformance {
                eprintln!(
                    "\n\n========== Running error reporting test {}\nSource:\n{}\n",
                    file_name, input
                );
            } else {
                eprintln!(
                    "\n\n========== Running test {}\nSource:\n{}\n",
                    file_name, input
                );
            }

            let path = dir.join(&file_name);
            do_test(false, &path, mode).unwrap();
        });
    }

    Ok(())
}

fn do_test(treat_error_as_bug: bool, file_name: &Path, mode: Mode) -> Result<(), StdErr> {
    let fname = file_name.display().to_string();
    let lines = match mode {
        Mode::Conformance => {
            let mut buf = String::new();
            File::open(file_name)
                .expect("failed to open file for testing")
                .read_to_string(&mut buf)
                .expect("failed to read file's content");

            Some(
                buf.lines()
                    .enumerate()
                    .filter(|(_, s)| s.contains("// error") || s.contains("// Error"))
                    .map(|(i, s)| (i, String::from(s)))
                    .map(|(i, _)| i + 1)
                    .collect::<Vec<_>>(),
            )
        }
        _ => None,
    };

    let res = ::testing::run_test(treat_error_as_bug, |cm, handler| {
        CM.set(&cm.clone(), || {
            let (libs, rule, ts_config) = {
                match mode {
                    Mode::Pass | Mode::Error => Default::default(),
                    Mode::Conformance => {
                        // We parse files twice. At first, we read comments and detect
                        // configurations for following parse.

                        let session = Session { handler: &handler };

                        let fm = cm.load_file(file_name).expect("failed to read file");
                        let comments = Comments::default();

                        let mut parser = Parser::new(
                            session,
                            Syntax::Typescript(TsConfig {
                                tsx: fname.contains("tsx"),
                                ..Default::default()
                            }),
                            SourceFileInput::from(&*fm),
                            Some(&comments), // Disable comments
                        );

                        let module = parser
                            .parse_module()
                            .map_err(|mut e| {
                                e.emit();
                                ()
                            })
                            .expect("failed to parser module");
                        let module = if mode == Mode::Conformance {
                            make_test(&comments, module)
                        } else {
                            module
                        };

                        let mut libs = vec![Lib::Es5];
                        let mut rule = Rule::default();
                        let ts_config = TsConfig::default();

                        let span = module.span;
                        let cmts = comments.leading_comments(span.lo());
                        match cmts {
                            Some(ref cmts) => {
                                for cmt in cmts.iter() {
                                    let s = cmt.text.trim();
                                    if !s.starts_with("@") {
                                        continue;
                                    }

                                    if s.starts_with("@target: ") {
                                        libs = Lib::load(&s[8..].trim());
                                    } else if s.starts_with("@strict: ") {
                                        let strict = s[8..].trim().parse().unwrap(); // TODO
                                        rule.no_implicit_any = strict;
                                        rule.no_implicit_this = strict;
                                        rule.always_strict = strict;
                                        rule.strict_null_checks = strict;
                                        rule.strict_function_types = strict;
                                    } else {
                                        panic!("Comment is not handled: {}", s);
                                    }
                                }
                            }
                            None => {}
                        }

                        (libs, rule, ts_config)
                    }
                }
            };

            let checker = swc_ts_checker::Checker::new(
                cm.clone(),
                handler,
                libs,
                rule,
                TsConfig {
                    tsx: fname.contains("tsx"),
                    ..ts_config
                },
            );

            let errors = checker.check(file_name.into());
            if let Some(count) = lines.as_ref().map(|v| v.len()) {
                if count != errors.len() {
                    checker.run(|| {
                        for e in errors {
                            e.emit(&handler);
                        }
                    });
                    return Err(());
                }
            }

            let res = if errors.is_empty() { Ok(()) } else { Err(()) };

            checker.run(|| {
                for e in errors {
                    e.emit(&handler);
                }
            });

            res
        })
    });

    match mode {
        Mode::Error => {
            let err = res.expect_err("should fail, but parsed as");
            if err
                .compare_to_file(format!("{}.stderr", file_name.display()))
                .is_err()
            {
                panic!()
            }
        }
        Mode::Pass => {
            res.expect("should be parsed and validated");
        }
        Mode::Conformance => {
            let err = match res {
                Ok(_) => StdErr::from(String::from("")),
                Err(err) => err,
            };

            // TODO: filter line correctly
            let mut err_lines = err
                .lines()
                .enumerate()
                .filter(|(_, l)| l.contains("$DIR"))
                .inspect(|(i, v)| println!("Line:({}) {}", i, v));

            let err_count = err_lines.clone().count();

            if err_count != lines.as_ref().unwrap().len() {
                panic!(
                    "{:?}\nExpected {} errors, got {}",
                    err,
                    lines.unwrap().len(),
                    err_count,
                );
            }

            let all = err_lines.all(|(_, v)| {
                for l in lines.as_ref().unwrap() {
                    if v.contains(&l.to_string()) {
                        return true;
                    }
                }
                false
            });

            if !all {
                panic!(
                    "{:?}\nExpected {} errors, got {}\nLines: {:?}\n",
                    err,
                    lines.as_ref().unwrap().len(),
                    err_count,
                    lines.as_ref().unwrap(),
                );
            }

            if err
                .compare_to_file(format!("{}.stderr", file_name.display()))
                .is_err()
            {
                panic!()
            }
        }
    }

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
                                    type_ann: box parse_type(cmt.span, &t),
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

fn parse_type(span: Span, s: &str) -> TsType {
    let s = s.trim();

    macro_rules! kwd {
        ($kind:expr) => {{
            return TsType::TsKeywordType(TsKeywordType { span, kind: $kind });
        }};
    }
    match s {
        "string" => kwd!(TsKeywordTypeKind::TsStringKeyword),
        "number" => kwd!(TsKeywordTypeKind::TsNumberKeyword),
        "undefined" => kwd!(TsKeywordTypeKind::TsUndefinedKeyword),
        "boolean" => kwd!(TsKeywordTypeKind::TsBooleanKeyword),
        _ => {}
    }

    if s.contains("|") {
        return TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(
            TsUnionType {
                span,
                types: s.split("|").map(|v| box parse_type(span, v)).collect(),
            },
        ));
    }

    unimplemented!("conformance test: parse_type({})", s)
}
