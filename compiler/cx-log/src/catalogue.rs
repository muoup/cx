use crate::error::{CXRawError, message::CXStdErrMessage};

pub mod analysis;
pub mod backend;
pub mod driver;
pub mod mir;
pub mod parse;
pub mod typecheck;

pub struct ErrorDefinition<T> {
    pub code: &'static str,
    pub message: fn(T) -> String,
}

impl<T> ErrorDefinition<T> {
    pub fn bind(&self, args: T) -> CXRawError {
        CXStdErrMessage::error(self.code, (self.message)(args))
    }
}

macro_rules! define_errors {
    ($($name:ident: $args:ty = $code:literal => $message:expr;)*) => {
        $(pub const $name: $crate::catalogue::ErrorDefinition<$args> =
            $crate::catalogue::ErrorDefinition { code: $code, message: $message };)*

        pub const CODES: &[&str] = &[$($code),*];
    };
}

pub(crate) use define_errors;
