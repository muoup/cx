pub mod error;
pub mod pretty;
pub mod span;

pub use error::{CXError, CXErrorTrait, CXResult, PointingError, UnderlineError, UnspannedError};
pub use pretty::*;
pub use span::{DiagnosticPointer, DiagnosticSpan, produce_diagnostic_error};

#[macro_export]
macro_rules! log_error {
    ($($arg:tt)*) => {
        {
            let msg = format!($($arg)*);
            eprintln!("Error: {}", msg);

            panic!()
        }
    }
}

#[macro_export]
macro_rules! expr_error_log {
    ($toks:expr, $start:expr, $end:expr, $($arg:tt)*) => {
        {
            use cx_log::log_error;

            eprintln!("{}", $toks[$start .. $end].iter().map(|tok| format!("{}", tok)).collect::<Vec<_>>().join(" "));
            log_error!($($arg)*);
        }
    }
}

#[macro_export]
macro_rules! lmir_error_log {
    ($builder:ident, $($arg:tt)*) => {
        {
            use cx_log::log_error;

            eprintln!("Error in method {}", $builder.current_function_name().unwrap_or("<unknown>"));
            log_error!($($arg)*);
        }
    }
}
