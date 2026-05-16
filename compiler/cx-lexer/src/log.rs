use cx_util::CXErrorTrait;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug)]
pub struct LexerError {
    pub message: String,
    pub file: PathBuf,
    pub source: String,
    pub start_index: usize,
    pub end_index: usize,
}

fn pretty_underline_error(
    message: &str,
    file_path: &Path,
    source: &str,
    start_index: usize,
    end_index: usize,
) {
    cx_log::pretty_underline_source_error(message, file_path, source, start_index, end_index);
}

impl CXErrorTrait for LexerError {
    fn pretty_print(&self) {
        pretty_underline_error(
            &self.error_message(),
            self.file.as_path(),
            &self.source,
            self.start_index,
            self.end_index,
        );
    }

    fn error_prefix(&self) -> String {
        "LEXER ERROR".to_string()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }

    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.file.clone())
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
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
            }) as Box<dyn cx_util::CXErrorTrait>)
        }
    };
}
