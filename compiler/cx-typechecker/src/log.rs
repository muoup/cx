use cx_log::{CXError, DiagnosticSpan};
use cx_tokens::{TokenRange, token::Token};

pub fn produce_typecheck_error(
    span: DiagnosticSpan,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXError> {
    cx_log::produce_diagnostic_error("TYPE ERROR", message, notes, span)
}

pub(crate) fn produce_comptime_error(
    span: DiagnosticSpan,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXError> {
    cx_log::produce_diagnostic_error("COMPTIME ERROR", message, notes, span)
}

pub fn produce_typecheck_error_for_range(
    tokens: &[Token],
    fallback_file: &std::path::Path,
    range: &TokenRange,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXError> {
    let span = range
        .to_diagnostic_span(tokens, fallback_file)
        .unwrap_or_else(|| DiagnosticSpan::new(fallback_file.to_owned(), 0, 1));

    produce_typecheck_error(span, message, notes)
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
            let engine = &$env;
            let range = &$range;
            let span = range
                .to_diagnostic_span(engine.source.tokens, engine.source.compilation_unit.as_path())
                .unwrap_or_else(|| cx_log::DiagnosticSpan::new(engine.source.compilation_unit.as_path(), 0, 1));

            $crate::log::produce_comptime_error(
                span,
                message,
                $notes,
            )
        }
    };

    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            let engine = &$env;
            let range = &$range;
            let span = range
                .to_diagnostic_span(engine.source.tokens, engine.source.compilation_unit.as_path())
                .unwrap_or_else(|| cx_log::DiagnosticSpan::new(engine.source.compilation_unit.as_path(), 0, 1));

            $crate::log::produce_comptime_error(
                span,
                message,
                Vec::new(),
            )
        }
    };
}

#[macro_export]
macro_rules! typecheck_error {
    ($env:expr, $range:expr, notes: $notes:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            // panic!("{}", message);

            let range = &$range;

            $crate::log::produce_typecheck_error_for_range(
                $env.source.tokens,
                $env.source.compilation_unit.as_path(),
                range,
                message,
                $notes,
            )
        }
    };

    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            // panic!("{}", message);

            let range = &$range;

            $crate::log::produce_typecheck_error_for_range(
                $env.source.tokens,
                $env.source.compilation_unit.as_path(),
                range,
                message,
                Vec::new(),
            )
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
