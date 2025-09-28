#[macro_export]
macro_rules! log_parse_error {
    ($data:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            cx_log::pretty_point_error(&message, &$data.tokens.file, &$data.tokens.slice[$data.tokens.index]);

            if cfg!(debug_assertions) {
                panic!("Parsing error: {}", message);
            } else {
                std::process::exit(1);
            }
        }
    };
}

#[macro_export]
macro_rules! log_preparse_error {
    ($toks:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);

            cx_log::pretty_point_error(&message, &$toks.file, &$toks.slice[$toks.index]);

            if cfg!(debug_assertions) {
                panic!("Preprocessing error: {}", message);
            } else {
                std::process::exit(1);
            }
        }
    };
}
