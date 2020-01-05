/// Visit with output
pub trait Validate<T> {
    type Output;

    fn validate(&mut self, node: &T) -> Self::Output;
}
