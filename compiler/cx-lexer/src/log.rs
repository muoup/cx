use cx_log::{CXError, CXErrorContext, CXErrorMessage};
use std::path::PathBuf;

#[derive(Clone, Debug)]
pub struct LexerError {
    pub message: String,
    pub file: PathBuf,
    pub source: String,
    pub start_index: usize,
    pub end_index: usize,
}

impl CXError for LexerError {}

impl CXErrorContext for LexerError {
    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.file.clone())
    }

    fn byte_start(&self) -> Option<usize> {
        Some(self.start_index)
    }

    fn byte_end(&self) -> Option<usize> {
        Some(self.end_index)
    }
}

impl CXErrorMessage for LexerError {
    fn error_prefix(&self) -> String {
        "LEXER ERROR".to_string()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }
}

#[macro_export]
macro_rules! log_lexer_error {
    ($file:expr, $source:expr, $start:expr, $end:expr, $($arg:tt)*) => {
        {
            Err(Box::new($crate::log::LexerError {
                message: format!($($arg)*),
                file: std::path::PathBuf::from($file),
                source: $source.to_string(),
                start_index: $start,
                end_index: $end,
            }) as Box<dyn cx_log::CXError>)
        }
    };
}
