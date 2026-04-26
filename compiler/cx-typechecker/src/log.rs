use cx_util::CXErrorTrait;
use std::path::PathBuf;

use cx_tokens::TokenRange;
use cx_tokens::token::{Token, TokenKind};

#[derive(Clone, Debug)]
pub struct TypeError {
    pub compilation_unit: PathBuf,
    pub token_start: usize,
    pub token_end: usize,
    pub byte_start: usize,
    pub byte_end: usize,
    pub message: String,
    pub notes: Vec<String>,
}

pub trait TypeErrorRangeArg {
    fn to_range(&self) -> Option<TokenRange>;
}

impl TypeErrorRangeArg for &TokenRange {
    fn to_range(&self) -> Option<TokenRange> {
        Some((*self).clone())
    }
}

impl TypeErrorRangeArg for Option<&TokenRange> {
    fn to_range(&self) -> Option<TokenRange> {
        self.cloned()
    }
}

impl TypeErrorRangeArg for Option<TokenRange> {
    fn to_range(&self) -> Option<TokenRange> {
        self.clone()
    }
}

impl CXErrorTrait for TypeError {
    fn pretty_print(&self) {
        cx_log::pretty_underline_error_with_notes(
            &self.error_message(),
            &self.notes,
            self.compilation_unit.as_path(),
            self.byte_start,
            self.byte_end,
        );
    }

    fn error_prefix(&self) -> String {
        "TYPE ERROR".to_string()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }

    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.compilation_unit.clone())
    }

    fn token_start(&self) -> Option<usize> {
        Some(self.token_start)
    }

    fn token_end(&self) -> Option<usize> {
        Some(self.token_end)
    }

    fn notes(&self) -> Vec<String> {
        self.notes.clone()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub fn byte_range_for_tokens(
    tokens: &[Token],
    start_token: usize,
    end_token: usize,
) -> (usize, usize) {
    let Some(start) = tokens.get(start_token) else {
        return (0, 1);
    };
    let end = tokens
        .get(end_token.saturating_sub(1))
        .map(|token| token.byte_end_index)
        .unwrap_or(start.byte_end_index);

    (
        start.byte_start_index,
        end.max(start.byte_start_index.saturating_add(1)),
    )
}

pub fn file_origin_for_tokens(
    tokens: &[Token],
    start_token: usize,
    end_token: usize,
) -> Option<PathBuf> {
    tokens
        .get(start_token)
        .or_else(|| end_token.checked_sub(1).and_then(|index| tokens.get(index)))
        .and_then(|token| {
            (!token.file_origin.as_os_str().is_empty())
                .then(|| PathBuf::from(token.file_origin.as_ref()))
        })
}

pub fn identifier_range_for_name(
    tokens: &[Token],
    fallback: &TokenRange,
    name: &str,
) -> TokenRange {
    tokens
        .get(fallback.start_token..fallback.end_token)
        .and_then(|range_tokens| {
            range_tokens
                .iter()
                .position(|token| matches!(&token.kind, TokenKind::Identifier(identifier) if identifier == name))
                .map(|offset| fallback.start_token + offset)
        })
        .and_then(|index| {
            tokens.get(index).map(|token| {
                TokenRange::new(
                    index,
                    index + 1,
                    std::sync::Arc::from(token.file_origin.to_string_lossy().as_ref()),
                )
            })
        })
        .unwrap_or_else(|| fallback.clone())
}

#[macro_export]
macro_rules! log_typecheck_error {
    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            // panic!("{}", message);

            let range = $crate::log::TypeErrorRangeArg::to_range(&$range);

            let (start_token, end_token) = if let Some(range) = range.as_ref() {
                (range.start_token, range.end_token)
            } else {
                (0, 0)
            };
            let compilation_unit = range
                .as_ref()
                .and_then(|range| {
                    (!range.file_origin.is_empty()).then(|| std::path::PathBuf::from(range.file_origin.as_ref()))
                })
                .or_else(|| $crate::log::file_origin_for_tokens($env.source.tokens, start_token, end_token))
                .unwrap_or_else(|| $env.source.compilation_unit.as_path().to_owned());
            let (byte_start, byte_end) =
                $crate::log::byte_range_for_tokens($env.source.tokens, start_token, end_token);

            Err(Box::new($crate::log::TypeError {
                message,
                token_start: start_token,
                token_end: end_token,
                byte_start,
                byte_end,
                compilation_unit,
                notes: Vec::new(),
            }) as Box<dyn cx_util::CXErrorTrait>)
        }
    };
}
