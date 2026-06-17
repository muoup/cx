use cx_log::{CXErrorTrait, DiagnosticSpan};

pub(crate) fn produce_comptime_error(
    span: Option<DiagnosticSpan>,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXErrorTrait> {
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
