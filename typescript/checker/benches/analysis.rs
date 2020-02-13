#![feature(test)]

extern crate test;

use std::{fs::File, path::PathBuf, sync::Arc};
use swc_common::input::SourceFileInput;
use swc_ecma_parser::{lexer::Lexer, Parser, Session, Syntax};
use swc_ts_checker::Checker;
use test::{black_box, Bencher};
use testing::Tester;

fn bench(b: &mut Bencher, s: &str) {
    b.bytes = File::open(s)
        .expect("failed ot open file")
        .metadata()
        .expect("failed to fetch metadata")
        .len();
    let path = Arc::new(PathBuf::from(s));

    let _ = Tester::new().errors(|cm, handler| {
        let module = {
            let session = Session { handler: &handler };

            let fm = cm.load_file(&path).expect("failed to read file");

            let lexer = Lexer::new(
                session,
                Syntax::Typescript(Default::default()),
                Default::default(),
                SourceFileInput::from(&*fm),
                None,
            );
            let mut parser = Parser::new_from(session, lexer);

            parser
                .parse_typescript_module()
                .map_err(|mut e| {
                    e.emit();
                })
                .expect("failed to parse module")
        };

        let c = Checker::new(
            cm,
            &handler,
            vec![],
            Default::default(),
            Default::default(),
            Default::default(),
        );
        b.iter(|| {
            black_box(c.analyze_module(path.clone(), &module));
        });

        Ok(())
    });
}

#[bench]
fn identity(b: &mut Bencher) {
    bench(b, "benches/identity.ts");
}

#[bench]
fn is_array(b: &mut Bencher) {
    bench(b, "benches/is_array.ts");
}

#[bench]
fn is_array_like(b: &mut Bencher) {
    bench(b, "benches/is_array_like.ts");
}

#[bench]
fn is_date(b: &mut Bencher) {
    bench(b, "benches/is_date.ts");
}

#[bench]
fn fn_try_catch(b: &mut Bencher) {
    bench(b, "benches/fn_try_catch.ts");
}

#[bench]
fn export_const(b: &mut Bencher) {
    bench(b, "benches/export_const.ts");
}

#[bench]
fn rxjs_internal_util_immediate(b: &mut Bencher) {
    bench(b, "benches/rxjs_internal_util_immediate.ts");
}

#[bench]
fn rxjs_internal_util_argument_out_of_range_error(b: &mut Bencher) {
    bench(
        b,
        "benches/rxjs_internal_util_argument_out_of_range_error.ts",
    );
}

#[bench]
fn rxjs_internal_util_empty_error(b: &mut Bencher) {
    bench(b, "benches/rxjs_internal_util_empty_error.ts");
}

#[bench]
fn rxjs_internal_util_apply_mixins(b: &mut Bencher) {
    bench(b, "benches/rxjs_internal_util_apply_mixins.ts");
}

#[bench]
fn rxjs_internal_util_error_object(b: &mut Bencher) {
    bench(b, "benches/rxjs_internal_util_error_object.ts");
}

#[bench]
fn rxjs_internal_util_host_report_error(b: &mut Bencher) {
    bench(b, "benches/rxjs_internal_util_host_report_error.ts");
}
