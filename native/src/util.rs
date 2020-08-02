use napi::Status;

/// Utility to convert [anyhow::Error] to [napi::Error]
pub trait ErrorExt<T>: Into<Result<T, anyhow::Error>> {
    fn convert_err(self, status: Status) -> Result<T, napi::Error> {
        match self.into() {
            Ok(v) => Ok(v),
            Err(err) => Err(napi::Error {
                status,
                reason: format!("{:?}", err),
            }),
        }
    }
}

impl<T> ErrorExt<T> for Result<T, anyhow::Error> {}
