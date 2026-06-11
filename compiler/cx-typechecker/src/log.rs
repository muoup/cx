use std::path::{Path, PathBuf};

use cx_log::UnderlineError;
use cx_tokens::token::Token;
use cx_tokens::{TokenRange, byte_range_for_tokens, file_origin_for_tokens};

pub trait TypecheckErrorRangeArg {
    fn to_range(&self) -> Option<TokenRange>;
}

impl TypecheckErrorRangeArg for &TokenRange {
    fn to_range(&self) -> Option<TokenRange> {
        Some((*self).clone())
    }
}

impl TypecheckErrorRangeArg for Option<&TokenRange> {
    fn to_range(&self) -> Option<TokenRange> {
        self.cloned()
    }
}

impl TypecheckErrorRangeArg for Option<TokenRange> {
    fn to_range(&self) -> Option<TokenRange> {
        self.clone()
    }
}

pub fn type_error_for_range(
    tokens: &[Token],
    fallback_file: &Path,
    range: &TokenRange,
    message: String,
    notes: Vec<String>,
) -> UnderlineError {
    let compilation_unit = (!range.file_origin.is_empty())
        .then(|| PathBuf::from(range.file_origin.as_ref()))
        .or_else(|| file_origin_for_tokens(tokens, range.start_token, range.end_token))
        .unwrap_or_else(|| fallback_file.to_owned());
    let (byte_start, byte_end) = byte_range_for_tokens(tokens, range.start_token, range.end_token);

    UnderlineError::new(
        "TYPE ERROR",
        message,
        compilation_unit,
        byte_start,
        byte_end,
    )
    .with_token_range(range.start_token, range.end_token)
    .with_notes(notes)
}

pub fn type_error_for_optional_range(
    tokens: &[Token],
    fallback_file: &Path,
    range: Option<&TokenRange>,
    message: String,
    notes: Vec<String>,
) -> Box<dyn cx_log::CXErrorTrait> {
    let Some(range) = range.filter(|range| !range.is_empty()) else {
        return Box::new(cx_log::UnspannedError::new("TYPE ERROR", message).with_notes(notes));
    };

    Box::new(type_error_for_range(
        tokens,
        fallback_file,
        range,
        message,
        notes,
    ))
}

#[macro_export]
macro_rules! typecheck_error {
    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            // panic!("{}", message);

            let range = $crate::log::TypecheckErrorRangeArg::to_range(&$range)
                .filter(|range| !range.is_empty());

            $crate::log::type_error_for_optional_range(
                $env.source.tokens,
                $env.source.compilation_unit.as_path(),
                range.as_ref(),
                message,
                Vec::new(),
            )
        }
    };
}

#[macro_export]
macro_rules! log_typecheck_error {
    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            use $crate::typecheck_error;

            Err(typecheck_error!($env, $range, $($arg)*))
        }
    };
}
