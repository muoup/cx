use std::path::Path;

use cx_log::{DiagnosticSpan, PointingError, UnderlineError};
use cx_tokens::token::Token;
use cx_tokens::{diagnostic_pointer_for_token, TokenRange};

pub fn pointing_error(
    default_file: &Path,
    token: Token,
    previous_token: Option<Token>,
    message: String,
) -> PointingError {
    let pointer = diagnostic_pointer_for_token(default_file, &token, previous_token.as_ref());

    PointingError::new("PARSER ERROR", message, pointer)
}

pub fn underline_error(
    default_file: &Path,
    tokens: &[Token],
    range: &TokenRange,
    message: String,
) -> UnderlineError {
    let span = range
        .to_diagnostic_span(tokens, default_file)
        .unwrap_or_else(|| DiagnosticSpan::new(default_file.to_owned(), 0, 1));

    UnderlineError::new("PARSER ERROR", message, span)
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
            )) as Box<dyn cx_log::CXError>)
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
            )) as Box<dyn cx_log::CXError>)
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
            )) as Box<dyn cx_log::CXError>)

        }
    };
}
