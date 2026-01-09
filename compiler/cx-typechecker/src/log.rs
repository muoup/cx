use std::path::PathBuf;
use cx_util::CXErrorTrait;

#[derive(Clone, Debug)]
pub struct TypeError {
    pub compilation_unit: PathBuf,
    pub token_start: usize,
    pub token_end: usize,
    pub message: String,
}

impl CXErrorTrait for TypeError {
    fn pretty_print(&self) {
        cx_log::pretty_underline_error(
            &self.message,
            self.compilation_unit.as_path(),
            self.token_start,
            self.token_end,
        );
    }

    fn error_message(&self) -> String {
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[macro_export]
macro_rules! log_typecheck_error {
    ($env:expr, $expr:expr, $($arg:tt)*) => {
        {
            let message = format!("TYPE ERROR: {}", format!($($arg)*));
            
            Err(Box::new($crate::log::TypeError {
                message,
                token_start: $expr.start_index,
                token_end: $expr.end_index,
                compilation_unit: $env.compilation_unit.as_path().to_owned(),
            }) as Box<dyn cx_util::CXErrorTrait>)
        }
    };
}
