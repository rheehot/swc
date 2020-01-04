use super::Analyzer;
use crate::{errors::Error, ty::TypeParamInstantiation};
use swc_ecma_ast::TsTypeParamInstantiation;

prevent!(TsTypeParamInstantiation);

impl Analyzer<'_, '_> {
    pub(super) fn visit_ts_type_param_instantiation(
        &mut self,
        i: &TsTypeParamInstantiation,
    ) -> Result<TypeParamInstantiation, Error> {
        unimplemented!()
    }
}
