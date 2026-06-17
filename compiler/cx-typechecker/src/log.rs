use cx_log::{CXErrorContext, DiagnosticSpan};

pub fn produce_typecheck_error(
    span: Option<DiagnosticSpan>,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXErrorContext> {
    cx_log::produce_diagnostic_error("TYPE ERROR", message, notes, span)
}

pub(crate) fn produce_comptime_error(
    span: Option<DiagnosticSpan>,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXErrorContext> {
    cx_log::produce_diagnostic_error("COMPTIME ERROR", message, notes, span)
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
            let _engine = &$env;
            let _range = &$range;

            $crate::log::produce_comptime_error(
                None,
                message,
                $notes,
            )
        }
    };

    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            let _engine = &$env;
            let _range = &$range;

            $crate::log::produce_comptime_error(
                None,
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
