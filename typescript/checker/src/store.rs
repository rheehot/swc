use crate::{errors::Error, ty::Type};
use swc_atoms::JsWord;

pub trait Store {
    fn report_error(&mut self, err: Error);

    fn get_var(&self, name: &JsWord) -> &Type;

    fn store_var(&mut self, name: &JsWord, ty: Type) -> Result<(), Error>;

    fn get_type(&self, name: &JsWord) -> &Type;

    /// Should handle declaration merging.
    fn store_type(&mut self, name: &JsWord, ty: Type) -> Result<(), Error>;
}

impl<'a, S> Store for &'a mut S
where
    S: Store,
{
    fn report_error(&mut self, err: Error) {
        S::report_error(self, err)
    }

    fn get_var(&self, name: &JsWord) -> &Type {
        S::get_var(self, name)
    }

    fn store_var(&mut self, name: &JsWord, ty: Type) -> Result<(), Error> {
        S::store_var(self, name, ty)
    }

    fn get_type(&self, name: &JsWord) -> &Type {
        S::get_type(self, name)
    }

    fn store_type(&mut self, name: &JsWord, ty: Type) -> Result<(), Error> {
        S::store_type(self, name, ty)
    }
}
