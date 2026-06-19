use std::path::PathBuf;

use cx_log::{CXError, CXUnspannedError, DiagnosticSpan};
use cx_pipeline_data::db::ModuleData;
use cx_tokens::TokenRange;
use cx_util::namespace::EnvironmentNamespace;

pub fn convert_token_range(
    module_data: &ModuleData,
    current_namespace: &EnvironmentNamespace,
    range: &TokenRange,
) -> DiagnosticSpan {
    let namespace = range.namespace().unwrap_or(current_namespace);
    let tokens = module_data.lex_tokens.get(namespace);
    let fallback_file = module_data
        .unit_for_namespace(namespace)
        .map(|unit| unit.as_path().to_owned())
        .unwrap_or_else(|| PathBuf::from(namespace.identifier()));

    range
        .to_diagnostic_span(tokens.as_ref(), fallback_file.as_path())
        .unwrap_or_else(|| DiagnosticSpan::new(fallback_file, 0, 1))
}

pub fn produce_compile_error(
    prefix: &'static str,
    module_data: &ModuleData,
    current_namespace: &EnvironmentNamespace,
    range: &TokenRange,
    message: String,
    mut notes: Vec<String>,
) -> Box<dyn CXError> {
    match range {
        TokenRange::Source { .. } => cx_log::produce_diagnostic_error(
            prefix,
            message,
            notes,
            convert_token_range(module_data, current_namespace, range),
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
    current_namespace: &EnvironmentNamespace,
    range: &TokenRange,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXError> {
    produce_compile_error(
        "TYPE ERROR",
        module_data,
        current_namespace,
        range,
        message,
        notes,
    )
}

pub fn produce_comptime_error(
    module_data: &ModuleData,
    current_namespace: &EnvironmentNamespace,
    range: &TokenRange,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXError> {
    produce_compile_error(
        "COMPTIME ERROR",
        module_data,
        current_namespace,
        range,
        message,
        notes,
    )
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
                $engine.env.module_data,
                &$engine.env.current_namespace,
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
                &$env.current_namespace,
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
