#[macro_export]
macro_rules! log_parse_error {
    ($data:expr, $($arg:tt)*) => {
        {
            let message = format!("PARSER ERROR: {}", format!($($arg)*));

            if false {
                cx_log::pretty_point_error(&message, &$data.tokens.file, &$data.tokens.slice[$data.tokens.index]);
                panic!("Parsing error: {}", message);
            }
            
            Err(cx_util::CXError::new(message))
        }
    };
}

#[macro_export]
macro_rules! log_preparse_error {
    ($toks:expr, $($arg:tt)*) => {
        {
            let message = format!("PARSER ERROR: {}", format!($($arg)*));

            if false {
                cx_log::pretty_point_error(&message, &$toks.file, $toks.peek().unwrap());
                panic!("Preprocessing error: {}", message);
            }
            
            Err(cx_util::CXError::new(message))
        }
    };
}
