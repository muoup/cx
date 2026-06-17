use std::path::Path;

use cx_log::{CXErrorTrait, DiagnosticSpan};
use cx_tokens::TokenRangeArg;
use cx_tokens::token::Token;

pub fn produce_typecheck_error(
    span: Option<DiagnosticSpan>,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXErrorTrait> {
    cx_log::produce_diagnostic_error("TYPE ERROR", message, notes, span)
}

pub fn type_error_for_optional_range(
    tokens: &[Token],
    fallback_file: &Path,
    range: impl TokenRangeArg,
    message: String,
    notes: Vec<String>,
) -> Box<dyn cx_log::CXErrorTrait> {
    produce_typecheck_error(
        range.to_diagnostic_span(tokens, fallback_file),
        message,
        notes,
    )
}

pub fn type_error_result_for_range<T>(
    tokens: &[Token],
    fallback_file: &Path,
    range: impl TokenRangeArg,
    message: String,
    notes: Vec<String>,
) -> cx_log::CXResult<T> {
    Err(produce_typecheck_error(
        range.to_diagnostic_span(tokens, fallback_file),
        message,
        notes,
    ))
}

#[macro_export]
macro_rules! typecheck_error {
    ($env:expr, $range:expr, notes: $notes:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            // panic!("{}", message);

            let span = cx_tokens::TokenRangeArg::to_diagnostic_span(
                &$range,
                $env.source.tokens,
                $env.source.compilation_unit.as_path(),
            );

            $crate::log::produce_typecheck_error(
                span,
                message,
                $notes,
            )
        }
    };

    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            // panic!("{}", message);

            let span = cx_tokens::TokenRangeArg::to_diagnostic_span(
                &$range,
                $env.source.tokens,
                $env.source.compilation_unit.as_path(),
            );

            $crate::log::produce_typecheck_error(
                span,
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
