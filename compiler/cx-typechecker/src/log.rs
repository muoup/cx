use cx_log::{CXError, DiagnosticSpan};
use cx_tokens::{TokenRange, token::Token};

pub fn convert_token_range(
    tokens: &[Token],
    fallback_file: &std::path::Path,
    range: &TokenRange
) -> DiagnosticSpan {
    range
        .to_diagnostic_span(tokens, fallback_file)
        .unwrap_or_else(|| DiagnosticSpan::new(fallback_file.to_owned(), 0, 1))
}

pub fn produce_typecheck_error(
    tokens: &[Token],
    fallback_file: &std::path::Path,
    range: &TokenRange,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXError> {
    cx_log::produce_diagnostic_error("TYPE ERROR", message, notes, convert_token_range(tokens, fallback_file, range))
}

pub fn produce_comptime_error(
    tokens: &[Token],
    fallback_file: &std::path::Path,
    range: &TokenRange,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXError> {
    cx_log::produce_diagnostic_error("COMPTIME ERROR", message, notes, convert_token_range(tokens, fallback_file, range))
}

#[macro_export]
macro_rules! log_comptime_error {
    ($env:expr, $range:expr, notes: $notes:expr, $($arg:tt)*) => {
        {
            Err($crate::comptime_error!($env, $range, notes: $notes, $($arg)*))
        }
    };

    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            Err($crate::comptime_error!($env, $range, $($arg)*))
        }
    };
}

#[macro_export]
macro_rules! comptime_error {
    ($env:expr, $range:expr, notes: $notes:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            $crate::log::produce_comptime_error(
                span,
                message,
                $notes,
            )
        }
    };

    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            $crate::comptime_error!($env, $range, notes: Vec::new(), $($arg)*)
        }
    };
}

#[macro_export]
macro_rules! typecheck_error {
    ($env:expr, $range:expr, notes: $notes:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            $crate::log::produce_typecheck_error(
                $env.source.tokens,
                $env.source.compilation_unit.as_path(),
                &$range,
                message,
                $notes,
            )
        }
    };

    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            $crate::typecheck_error!($env, $range, notes: Vec::new(), $($arg)*)
        }
    };
}

#[macro_export]
macro_rules! log_typecheck_error {
    ($env:expr, $range:expr, notes: $notes:expr, $($arg:tt)*) => {
        {
            use $crate::typecheck_error;

            Err(typecheck_error!($env, $range, notes: $notes, $($arg)*))
        }
    };

    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            use $crate::typecheck_error;

            Err(typecheck_error!($env, $range, $($arg)*))
        }
    };
}
