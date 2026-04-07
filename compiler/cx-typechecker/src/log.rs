use std::path::PathBuf;
use cx_util::CXErrorTrait;

#[derive(Clone, Debug)]
pub struct TypeError {
    pub compilation_unit: PathBuf,
    pub token_start: usize,
    pub token_end: usize,
    pub message: String,
    pub notes: Vec<String>,
}

impl CXErrorTrait for TypeError {
    fn pretty_print(&self) {
        cx_log::pretty_underline_error_with_notes(
            &self.error_message(),
            &self.notes,
            self.compilation_unit.as_path(),
            self.token_start,
            self.token_end,
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

#[macro_export]
macro_rules! log_typecheck_error {
    ($env:expr, $range:expr, $($arg:tt)*) => {
        {
            let message = format!("TYPE ERROR: {}", format!($($arg)*));
            let range: &cx_tokens::TokenRange = $range;

            let compilation_unit = if range.file_origin.is_empty() {
                $env.compilation_unit.as_path().to_owned()
            } else {
                std::path::PathBuf::from(range.file_origin.as_ref())
            };

            Err(Box::new($crate::log::TypeError {
                message,
                token_start: range.start_token,
                token_end: range.end_token,
                compilation_unit,
                notes: Vec::new(),
            }) as Box<dyn cx_util::CXErrorTrait>)
        }
    };
}
