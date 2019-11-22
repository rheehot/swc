macro_rules! analyze {
    ($a:expr, $e:expr) => {{
        let res: Result<(), $crate::errors::Error> = try { $e };

        match res {
            Ok(()) => {}
            Err(err) => $a.info.errors.push(err),
        }
    }};
}

pub fn type_name<T>(_: &T) -> &'static str {
    ::std::any::type_name::<T>()
}

macro_rules! log_fold {
    ($e:expr) => {{
        let ty_name = crate::analyzer::macros::type_name(&$e);
        if crate::analyzer::LOG_VISIT {
            println!("Fold<{}>", ty_name);
        }
    }};
}
