use crate::error::{CXRawError, message::CXStdErrMessage};

pub mod analysis;
pub mod backend;
pub mod driver;
pub mod mir;
pub mod parse;
pub mod typecheck;

pub const ISSUE_TRACKER: &str = "https://github.com/muoup/cx/issues";

// Internal errors put an 'X' after their stage prefix (e.g. 'MX001'): they report a compiler bug
// rather than a problem with the input
pub fn is_internal(code: &str) -> bool {
    code.as_bytes().get(1) == Some(&b'X')
}

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
