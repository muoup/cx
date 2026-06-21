use std::path::PathBuf;

use cx_log::{CXUnspannedError, DiagnosticSpan};
use cx_pipeline_data::db::ModuleData;
use cx_tokens::TokenRange;

pub fn convert_token_range(module_data: &ModuleData, range: &TokenRange) -> DiagnosticSpan {
    
}

pub fn produce_compile_error(
    prefix: &'static str,
    module_data: &ModuleData,
    range: &TokenRange,
    message: String,
    mut notes: Vec<String>,
) -> CXErr {
    match range {
        TokenRange::Source { .. } => cx_log::produce_diagnostic_error(
            prefix,
            message,
            notes,
            convert_token_range(module_data, range),
        ),
        TokenRange::Internal => {
            notes.push("diagnostic originated in compiler-generated code".to_string());
            Box::new(CXUnspannedError::new(prefix, message).with_notes(notes))
        }
        TokenRange::Error(range_error) => {
            notes.push(format!("failed to determine source range: {range_error}"));
            Box::new(CXUnspannedError::new(prefix, message).with_notes(notes))
        }
    }
}

pub fn produce_typecheck_error(
    module_data: &ModuleData,
    range: &TokenRange,
    message: String,
    notes: Vec<String>,
) -> CXErr {
    produce_compile_error("TYPE ERROR", module_data, range, message, notes)
}

pub fn produce_comptime_error(
    module_data: &ModuleData,
    range: &TokenRange,
    message: String,
    notes: Vec<String>,
) -> CXErr {
    produce_compile_error("COMPTIME ERROR", module_data, range, message, notes)
}

#[macro_export]
macro_rules! log_comptime_error {
    ($engine:expr, $range:expr, notes: $notes:expr, $($arg:tt)*) => {
        {
            Err($crate::comptime_error!($engine, $range, notes: $notes, $($arg)*))
        }
    };

    ($engine:expr, $range:expr, $($arg:tt)*) => {
        {
            Err($crate::comptime_error!($engine, $range, $($arg)*))
        }
    };
}

#[macro_export]
macro_rules! comptime_error {
    ($engine:expr, $range:expr, notes: $notes:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            $crate::log::produce_comptime_error(
                $engine.env().module_data,
                &$range,
                message,
                $notes,
            )
        }
    };

    ($engine:expr, $range:expr, $($arg:tt)*) => {
        {
            $crate::comptime_error!($engine, $range, notes: Vec::new(), $($arg)*)
        }
    };
}

#[macro_export]
macro_rules! typecheck_error {
    ($env:expr, $range:expr, notes: $notes:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            $crate::log::produce_typecheck_error(
                $env.module_data,
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

            Err(typecheck_error!($env, $range, notes: $notes, $($arg)*).into())
        }
    };

    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            use $crate::typecheck_error;

            Err(typecheck_error!($env, $range, $($arg)*).into())
        }
    };
}
