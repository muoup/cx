use std::path::PathBuf;

use cx_tokens::token::Token;
use cx_util::CXErrorTrait;

pub struct ParseErrorLog {
    pub message: String,
    pub file: PathBuf,
    pub token: Token
}

impl CXErrorTrait for ParseErrorLog {
    fn pretty_print(&self) {
        cx_log::pretty_point_error(&self.message, &self.file, &self.token);
    }
}

#[macro_export]
macro_rules! log_parse_error {
    ($data:expr, $($arg:tt)*) => {
        {
            let message = format!("PARSER ERROR: {}", format!($($arg)*));

            Err(Box::new($crate::log::ParseErrorLog {
                message,
                file: $data.tokens.file.clone(),
                token: $data.tokens.slice[$data.tokens.index].clone(),
            }) as Box<dyn cx_util::CXErrorTrait>)
        }
    };
}

#[macro_export]
macro_rules! log_preparse_error {
    ($toks:expr, $($arg:tt)*) => {
        {
            let message = format!("PARSER ERROR: {}", format!($($arg)*));

            Err(Box::new($crate::log::ParseErrorLog {
                message,
                file: $toks.file.clone(),
                token: $toks.peek().unwrap().clone(),
            }) as Box<dyn cx_util::CXErrorTrait>)
        } 
    };
}
