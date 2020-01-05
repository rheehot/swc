use std::ops::Try;
use swc_common::VisitWith;

/// Visit with output
pub trait Validate<T: ?Sized> {
    type Output: Output;

    fn validate(&mut self, node: &T) -> Self::Output;
}

impl<T, V> Validate<T> for V
where
    T: VisitWith<Self>,
{
    default type Output = ();

    default fn validate(&mut self, node: &T) -> Self::Output {
        node.validate_children(self);
        Self::Output::unit()
    }
}

impl<T, V> Validate<Box<T>> for V
where
    T: VisitWith<Self>,
    Self: Validate<T>,
{
    type Output = <Self as Validate<T>>::Output;

    fn validate(&mut self, node: &Box<T>) -> Self::Output {
        self.validate(&**node)
    }
}

impl<T, V> Validate<Option<T>> for V
where
    T: VisitWith<Self>,
    Self: Validate<T>,
{
    type Output = Option<<Self as Validate<T>>::Output>;

    fn validate(&mut self, node: &Option<T>) -> Self::Output {
        match node {
            Some(ref n) => Some(self.validate(n)),
            None => None,
        }
    }
}

impl<T, V, O, E> Validate<[T]> for V
where
    T: VisitWith<Self>,
    Self: Validate<T, Output = Result<O, E>>,
{
    type Output = Result<Vec<O>, E>;

    fn validate(&mut self, nodes: &[T]) -> Self::Output {
        let mut outputs = Vec::with_capacity(nodes.len());
        for node in nodes {
            outputs.push(self.validate(node));
        }
        outputs
    }
}

impl<'a, T, V> Validate<&'a T> for V
where
    T: VisitWith<Self>,
    Self: Validate<T>,
{
    type Output = <Self as Validate<T>>::Output;

    fn validate(&mut self, node: &&T) -> Self::Output {
        self.validate(&**node)
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
        v.validate(self)
    }

    fn validate_children(&self, v: &mut V) {
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
