/// Verifies that the first argument does not have a dummy span.
macro_rules! no_dummy {
    ($e:expr) => {{
        let span = ::swc_common::Spanned::span(&$e);
        if span.is_dummy() {
            unreachable!("{:?} should not have a dummy span", $e)
        }
    }};
}
