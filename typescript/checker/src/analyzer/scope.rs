pub(super) struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
}
