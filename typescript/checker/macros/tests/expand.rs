use std::marker::PhantomData;
use swc_ts_checker_macros::validator;

#[derive(Debug)]
struct Expr;

#[derive(Debug)]
struct Error;

struct Analyzer<'a, 'b> {
    phantom: PhantomData<&'a &'b ()>,
}

#[validator]
impl Validate<Expr> for Analyzer<'_, '_> {
    type Output = ValidationResult;

    fn validate(&mut self, e: &Expr) -> ValidationResult {}
}
