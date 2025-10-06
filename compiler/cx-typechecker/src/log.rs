#[macro_export]
macro_rules! log_typecheck_error {
    ($env:expr, $expr:expr, $($arg:tt)*) => {
        {
            let message = format!("TYPE ERROR: {}", format!($($arg)*));

            cx_log::pretty_underline_error(&message, $env.current_file, $env.tokens, $expr.start_index, $expr.end_index);

            if cfg!(debug_assertions) {
                panic!("Typechecking error: {}", message);
            } else {
                std::process::exit(1);
            }
        }
    };
}
