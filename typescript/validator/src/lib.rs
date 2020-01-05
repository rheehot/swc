#![feature(specialization)]
#![feature(box_syntax)]

use std::any::type_name;
use swc_common::{Visit, VisitWith, DUMMY_SP};
use swc_ecma_ast::*;

/// Visit with output
pub trait Validate<T: ?Sized> {
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
        node.validate_children(self);
        Self::Output::unit()
    }
}

pub trait ValidateWith<V> {
    type Output;
    fn validate_with(&self, v: &mut V) -> Self::Output;

    fn validate_children(&self, v: &mut V);
}

impl<V, T> ValidateWith<V> for T
where
    V: Validate<T>,
    T: VisitWith<V>,
{
    type Output = V::Output;

    fn validate_with(&self, v: &mut V) -> Self::Output {
        println!("{}.validate_with", type_name::<T>());

        v.validate(self)
    }

    fn validate_children(&self, v: &mut V) {
        println!("{}.validate_children", type_name::<T>());

        self.visit_children(v);
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

impl Visit<Box<Expr>> for Analyzer {
    fn visit(&mut self, e: &Box<Expr>) {
        println!("Visit<{}>", type_name::<Box<Expr>>());

        e.visit_children(self);
    }
}

impl Visit<Expr> for Analyzer {
    fn visit(&mut self, e: &Expr) {
        println!("Visit<{}>", type_name::<Expr>());

        e.visit_children(self);
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
