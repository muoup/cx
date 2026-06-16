use std::path::{Path, PathBuf};

use cx_log::{PointingError, UnderlineError};
use cx_tokens::token::Token;
use cx_tokens::{byte_range_for_tokens, file_origin_for_tokens, TokenRange};

fn file_for_token(default_file: &Path, token: &Token) -> PathBuf {
    if token.file_origin.as_os_str().is_empty() {
        default_file.to_owned()
    } else {
        PathBuf::from(token.file_origin.as_ref())
    }
}

fn line_start_byte(contents: &str, index: usize) -> usize {
    let safe_index = index.min(contents.len());
    contents[..safe_index]
        .rfind('\n')
        .map(|idx| idx + 1)
        .unwrap_or(0)
}

fn line_content_start_byte(contents: &str, index: usize) -> usize {
    let line_start = line_start_byte(contents, index);
    let line_end = contents[line_start..]
        .find('\n')
        .map(|offset| line_start + offset)
        .unwrap_or(contents.len());
    let line = &contents[line_start..line_end];
    let first_non_whitespace = line
        .char_indices()
        .find(|(_, ch)| !ch.is_whitespace())
        .map(|(offset, _)| offset)
        .unwrap_or(0);

    line_start + first_non_whitespace
}

pub fn pointing_error(
    default_file: &Path,
    token: Token,
    previous_token: Option<Token>,
    message: String,
) -> PointingError {
    let file = file_for_token(default_file, &token);
    let anchor_token = previous_token.as_ref().unwrap_or(&token);
    let diagnostic_start = std::fs::read_to_string(&file)
        .ok()
        .map(|source| line_content_start_byte(&source, anchor_token.byte_start_index))
        .unwrap_or(anchor_token.byte_start_index);
    let diagnostic_end = anchor_token
        .byte_end_index
        .max(anchor_token.byte_start_index.saturating_add(1));

    PointingError::new("PARSER ERROR", message, file, token.byte_start_index)
        .with_diagnostic_range(diagnostic_start, diagnostic_end)
}

pub fn underline_error(
    default_file: &Path,
    tokens: &[Token],
    range: &TokenRange,
    message: String,
) -> UnderlineError {
    let file = (!range.file_origin.is_empty())
        .then(|| PathBuf::from(range.file_origin.as_ref()))
        .or_else(|| file_origin_for_tokens(tokens, range.start_token, range.end_token))
        .unwrap_or_else(|| default_file.to_owned());
    let (byte_start, byte_end) = byte_range_for_tokens(tokens, range.start_token, range.end_token);

    UnderlineError::new("PARSER ERROR", message, file, byte_start, byte_end)
        .with_token_range(range.start_token, range.end_token)
}

#[macro_export]
macro_rules! log_parse_error {
    ($data:expr, $($arg:tt)*) => {
        {
            let message = format!("{}", format!($($arg)*));

            // if cfg!(debug_assertions) {
            //     panic!();
            // }

            Err(Box::new($crate::log::pointing_error(
                $data.tokens.file.as_path(),
                $data.tokens.slice[$data.tokens.index].clone(),
                $data.tokens.prev().cloned(),
                message,
            )) as Box<dyn cx_log::CXErrorTrait>)
        }
    };
}

#[macro_export]
macro_rules! log_preparse_error {
    ($toks:expr, $($arg:tt)*) => {
        {
            let message = format!("{}", format!($($arg)*));
            let toks = &$toks;
            let token = toks.peek().cloned().or_else(|| toks.prev().cloned()).unwrap();

            Err(Box::new($crate::log::pointing_error(
                toks.file.as_path(),
                token,
                toks.prev().cloned(),
                message,
            )) as Box<dyn cx_log::CXErrorTrait>)
        }
    };
}

#[macro_export]
macro_rules! log_parse_underline_error {
    ($data:expr, $toks:expr, $($arg:tt)*) => {
        {
            let message = format!("{}", format!($($arg)*));

            Err(Box::new($crate::log::underline_error(
                $data.tokens.file.as_path(),
                $data.tokens.slice,
                $toks,
                message,
            )) as Box<dyn cx_log::CXErrorTrait>)

        }
    };
}
