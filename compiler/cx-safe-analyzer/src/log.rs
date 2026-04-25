use cx_util::CXErrorTrait;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug)]
pub struct AnalysisError {
    pub compilation_unit: PathBuf,
    pub token_start: usize,
    pub token_end: usize,
    pub byte_start: usize,
    pub byte_end: usize,
    pub message: String,
    pub notes: Vec<String>,
}

impl CXErrorTrait for AnalysisError {
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
        "ANALYSIS ERROR".to_string()
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

pub fn byte_range_for_source_tokens(
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
    let Some(start) = tokens.get(start_token) else {
        return (0, 1);
    };
    let end = tokens
        .get(end_token.saturating_sub(1))
        .map(|token| token.end_index)
        .unwrap_or(start.end_index);

    (
        start.start_index,
        end.max(start.start_index.saturating_add(1)),
    )
}

#[macro_export]
macro_rules! log_analysis_error {
    ($env:expr, $expr:expr, $($arg:tt)*) => {
        {
            let message = format!("{}", format!($($arg)*));

            let (token_start, token_end) = if let Some(token) = $expr.token_range.as_ref() {
                (token.start_token, token.end_token)
            } else {
                (0, 0) // Default to 0 if no token information is available
            };
            let range_file = $expr.token_range.as_ref().and_then(|range| {
                (!range.file_origin.is_empty()).then_some(std::path::PathBuf::from(range.file_origin.as_ref()))
            });
            let compilation_unit = range_file
                .as_ref()
                .unwrap_or(&$env.compilation_unit)
                .to_owned();
            let (byte_start, byte_end) =
                $crate::log::byte_range_for_source_tokens(compilation_unit.as_path(), token_start, token_end);

            Err(Box::new($crate::log::AnalysisError {
                message,
                token_start,
                token_end,
                byte_start,
                byte_end,
                compilation_unit,
                notes: Vec::new(),
            }) as Box<dyn cx_util::CXErrorTrait>)
        }
    };
}
