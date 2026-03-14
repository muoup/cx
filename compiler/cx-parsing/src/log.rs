use std::path::PathBuf;

use cx_tokens::token::Token;
use cx_util::CXErrorTrait;

#[derive(Clone, Debug)]
pub struct ParseErrorLog {
    pub message: String,
    pub file: PathBuf,
    pub token: Token,
    pub previous_token: Option<Token>,
}

impl CXErrorTrait for ParseErrorLog {
    fn pretty_print(&self) {
        cx_log::pretty_point_error(&self.message, &self.file, &self.token);
    }

    fn error_message(&self) -> String {
        format!("PARSER ERROR: {}", self.message)
    }

    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.file.clone())
    }

    fn token_start(&self) -> Option<usize> {
        Some(self.token.start_index)
    }

    fn token_end(&self) -> Option<usize> {
        Some(self.token.end_index)
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[macro_export]
macro_rules! log_parse_error {
    ($data:expr, $($arg:tt)*) => {
        {
            let message = format!("{}", format!($($arg)*));
            
            // if cfg!(debug_assertions) {
            //     panic!();
            // }

            Err(Box::new($crate::log::ParseErrorLog {
                message,
                file: $data.tokens.file.clone(),
                token: $data.tokens.slice[$data.tokens.index].clone(),
                previous_token: $data.tokens.prev().cloned(),
            }) as Box<dyn cx_util::CXErrorTrait>)
        }
    };
}

#[macro_export]
macro_rules! log_preparse_error {
    ($toks:expr, $($arg:tt)*) => {
        {
            let message = format!("{}", format!($($arg)*));

            Err(Box::new($crate::log::ParseErrorLog {
                message,
                file: $toks.file.clone(),
                token: $toks.peek().unwrap().clone(),
                previous_token: $toks.prev().cloned(),
            }) as Box<dyn cx_util::CXErrorTrait>)
        } 
    };
}
