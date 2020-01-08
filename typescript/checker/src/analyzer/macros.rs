macro_rules! prevent {
    ($T:ty) => {
        /// Delegated to Validate.
        impl ::swc_common::Visit<$T> for Analyzer<'_, '_> {
            fn visit(&mut self, n: &$T) {
                use crate::{analyzer::util::ResultExt, validator::Validate};
                self.validate(n).store(&mut self.info.errors);
            }
        }
    };
}

macro_rules! try_opt {
    ($e:expr) => {{
        match $e {
            Some(v) => Some(v?),
            None => None,
        }
    }};
}
