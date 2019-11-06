macro_rules! analyze {
    ($a:expr, $e:expr) => {{
        let res: Result<(), $crate::errors::Error> = try { $e };

        match res {
            Ok(()) => {}
            Err(err) => $a.info.errors.push(err),
        }
    }};
}
