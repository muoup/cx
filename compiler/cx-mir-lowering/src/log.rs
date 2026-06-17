use std::path::{Path, PathBuf};

use cx_log::{CXErrorTrait, UnderlineError, UnspannedError};
use cx_tokens::{byte_range_for_tokens, token::TokenRange};

use crate::comptime::engine::ComptimeEngine;

pub(crate) trait ComptimeErrorRangeArg {
    fn to_range(&self) -> Option<TokenRange>;
}

impl ComptimeErrorRangeArg for &TokenRange {
    fn to_range(&self) -> Option<TokenRange> {
        Some((*self).clone())
    }
}

impl ComptimeErrorRangeArg for TokenRange {
    fn to_range(&self) -> Option<TokenRange> {
        Some(self.clone())
    }
}

impl ComptimeErrorRangeArg for Option<&TokenRange> {
    fn to_range(&self) -> Option<TokenRange> {
        self.cloned()
    }
}

impl ComptimeErrorRangeArg for Option<TokenRange> {
    fn to_range(&self) -> Option<TokenRange> {
        self.clone()
    }
}

pub(crate) fn produce_comptime_error(
    engine: &ComptimeEngine,
    range: Option<TokenRange>,
    message: String,
    notes: Vec<String>,
) -> Box<dyn CXErrorTrait> {
    let Some(range) = range.filter(|range| !range.is_empty()) else {
        return Box::new(UnspannedError::new("COMPTIME ERROR", message).with_notes(notes));
    };

    let compilation_unit = compilation_unit_for_range(engine.source_path(), &range);
    let (byte_start, byte_end) = byte_range_for_source_tokens(
        compilation_unit.as_path(),
        range.start_token,
        range.end_token,
    );

    Box::new(
        UnderlineError::new(
            "COMPTIME ERROR",
            message,
            compilation_unit,
            byte_start,
            byte_end,
        )
        .with_token_range(range.start_token, range.end_token)
        .with_notes(notes),
    )
}

fn compilation_unit_for_range(fallback_file: &Path, range: &TokenRange) -> PathBuf {
    (!range.file_origin.is_empty())
        .then(|| PathBuf::from(range.file_origin.as_ref()))
        .unwrap_or_else(|| fallback_file.to_owned())
}

fn byte_range_for_source_tokens(
    file_path: &Path,
    start_token: usize,
    end_token: usize,
) -> (usize, usize) {
    let Ok(source) = std::fs::read_to_string(file_path) else {
        return (0, 1);
    };
    let Ok(tokens) = cx_lexer::lex(&source) else {
        return (0, 1);
    };

    byte_range_for_tokens(&tokens, start_token, end_token)
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

            let range = $crate::log::ComptimeErrorRangeArg::to_range(&$range)
                .filter(|range| !range.is_empty());

            $crate::log::produce_comptime_error(
                $env,
                range,
                message,
                $notes,
            )
        }
    };

    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            let range = $crate::log::ComptimeErrorRangeArg::to_range(&$range)
                .filter(|range| !range.is_empty());

            $crate::log::produce_comptime_error(
                $env,
                range,
                message,
                Vec::new(),
            )
        }
    };
}
