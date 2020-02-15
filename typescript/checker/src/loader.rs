use super::Checker;
use crate::{errors::Error, resolver::Resolve, ty::Type, Exports, ImportInfo, Specifier};
use fxhash::FxHashMap;
use std::{path::PathBuf, sync::Arc};
use swc_atoms::JsWord;

pub trait Load: Send + Sync {
    fn load(
        &self,
        base: Arc<PathBuf>,
        import: &ImportInfo,
    ) -> Result<Exports<FxHashMap<JsWord, Type>>, Error>;
}

impl Load for Checker<'_> {
    fn load(
        &self,
        base: Arc<PathBuf>,
        import: &ImportInfo,
    ) -> Result<Exports<FxHashMap<JsWord, Type>>, Error> {
        let mut result = Exports::default();
        let mut errors = vec![];

        let path = self
            .resolver
            .resolve((*base).clone(), import.span, &import.src)
            .map(Arc::new)?;
        let module = self.load_module(path);

        if import.all {
            result.extend(module.1.exports.clone())
        } else {
            for &Specifier {
                ref local,
                ref export,
            } in import.items.iter()
            {
                let mut done = false;
                if let Some(ty) = module.1.exports.types.get(&export.0) {
                    done = true;
                    result.types.insert(export.0.clone(), ty.clone());
                }

                if let Some(var) = module.1.exports.vars.get(&export.0) {
                    done = true;
                    result.vars.insert(export.0.clone(), var.clone());
                }

                if !done {
                    errors.push(local.clone());
                }
            }
        }

        if !module.1.errors.is_empty() {
            return Err(Error::ModuleLoadFailed {
                span: import.span,
                errors: module.1.errors.clone().into(),
            });
        }

        if errors.is_empty() {
            return Ok(result);
        }

        Err(Error::NoSuchExport {
            span: import.span,
            items: errors,
        })
    }
}

impl<'a, T> Load for &'a T
where
    T: ?Sized + Load,
{
    fn load(
        &self,
        base: Arc<PathBuf>,
        import: &ImportInfo,
    ) -> Result<Exports<FxHashMap<JsWord, Type>>, Error> {
        (**self).load(base, import)
    }
}
