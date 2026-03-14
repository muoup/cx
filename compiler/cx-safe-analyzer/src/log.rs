use std::path::PathBuf;
use cx_util::CXErrorTrait;

#[derive(Clone, Debug)]
pub struct AnalysisError {
    pub compilation_unit: PathBuf,
    pub token_start: usize,
    pub token_end: usize,
    pub message: String,
    pub notes: Vec<String>,
}

impl CXErrorTrait for AnalysisError {
    fn pretty_print(&self) {
        cx_log::pretty_underline_error_with_notes(
            &self.error_message(),
            &self.notes,
            self.compilation_unit.as_path(),
            self.token_start,
            self.token_end,
        );
    }

    fn error_message(&self) -> String {
        format!("ANALYSIS ERROR: {}", self.message)
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

#[macro_export]
macro_rules! log_analysis_error {
    ($env:expr, $expr:expr, $($arg:tt)*) => {
        {
            let message = format!("{}", format!($($arg)*));
            
            let (token_start, token_end) = if let Some(token) = $expr.source_range.as_ref() {
                (token.start_token, token.end_token)
            } else {
                (0, 0) // Default to 0 if no token information is available
            };
            
            Err(Box::new($crate::log::AnalysisError {
                message,
                token_start,
                token_end,
                compilation_unit: $env.compilation_unit.as_path().to_owned(),
                notes: Vec::new(),
            }) as Box<dyn cx_util::CXErrorTrait>)
        }
    };
}
