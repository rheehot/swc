macro_rules! prevent {
    ($T:ty) => {
        ///// # Note
        /////
        ///// This implementation **always panics**, and a dedicated method should be
        ///// called instead of visit<T>
        //impl ::swc_common::Visit<$T> for Analyzer<'_, '_> {
        //    fn visit(&mut self, n: &$T) {
        //        unreachable!(
        //            "Visit<{}> should be not called and {} should be handled by a
        // dedicated \     method\n{:?}",
        //            stringify!($T),
        //            stringify!($T),
        //            n
        //        );
        //    }
        //}
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
