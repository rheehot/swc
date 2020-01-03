use self::scope::Scope;

mod expr;
mod scope;
mod stmt;

#[derive(Debug)]
pub struct Analyzer<'a> {
    scope: Scope<'a>,
}
