use crate::{errors::Error, ty::Type};

pub trait Store {
    fn get_var(&self, name: &JsWord) -> &Type;

    fn store_var(&mut self, name: &JsWord, ty: Type) -> Result<(), Error>;

    fn get_type(&self, name: &JsWord) -> &Type;

    /// Should handle declaration merging.
    fn store_type(&mut self, name: &JsWord, ty: Type) -> Result<(), Error>;
}
