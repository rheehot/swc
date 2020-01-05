#![feature(specialization)]
#![feature(box_syntax)]

use std::any::type_name;
use swc_common::{Visit, VisitWith, DUMMY_SP};
use swc_ecma_ast::*;

/// Visit with output
pub trait Validate<T> {
    type Output: Output;

    fn validate(&mut self, node: &T) -> Self::Output;
}

#[derive(Debug, Default)]
struct Analyzer {
    idents: Vec<Ident>,
}

impl<T> Validate<T> for Analyzer
where
    Self: Visit<T>,
    T: VisitWith<Self>,
{
    default type Output = ();

    default fn validate(&mut self, node: &T) -> Self::Output {
        println!("Validate<{}> (default)", type_name::<T>());
        node.visit_children(self);
        Self::Output::unit()
    }
}

pub trait Output: Sized {
    fn unit() -> Self;
}

impl<T> Output for T {
    default fn unit() -> Self {
        unreachable!()
    }
}

impl Output for () {
    fn unit() -> Self {}
}

impl Visit<Ident> for Analyzer {
    fn visit(&mut self, node: &Ident) {
        println!("Visit<Ident>");
    }
}

impl Validate<Ident> for Analyzer {
    type Output = Result<(), ()>;

    fn validate(&mut self, node: &Ident) -> Self::Output {
        println!("Validate<Ident>");

        self.idents.push(node.clone());
        Ok(())
    }
}

impl Visit<WithStmt> for Analyzer {
    fn visit(&mut self, node: &WithStmt) {
        println!("Visit<WithStmt>");
    }
}

#[test]
fn usage() {
    let i = Ident::new("".into(), DUMMY_SP);

    let mut a = Analyzer::default();
    let res = a.validate(&i);

    assert_eq!(res, Ok(()));
    assert_eq!(a.idents.len(), 1);
}

#[test]
fn mixed_visit() {
    let stmt = Stmt::Expr(ExprStmt {
        span: DUMMY_SP,
        expr: box Expr::Ident(Ident::new("".into(), DUMMY_SP)),
    });

    let mut a = Analyzer::default();
    Validate::<Stmt>::validate(&mut a, &stmt);
    assert_eq!(a.idents.len(), 1);
    //    stmt.validate_with(&mut a);
}

#[test]
fn mixed_visit_2() {
    let stmt = Stmt::Expr(ExprStmt {
        span: DUMMY_SP,
        expr: box Expr::Ident(Ident::new("".into(), DUMMY_SP)),
    });

    let mut a = Analyzer::default();
    stmt.visit_with(&mut a);
    assert_eq!(a.idents.len(), 1);
    //    stmt.validate_with(&mut a);
}
