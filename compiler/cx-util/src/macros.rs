#[macro_export]
macro_rules! log_error {
    ($($arg:tt)*) => {
        {
            eprintln!($($arg)*);

            // If in debug mode, panic on error
            if cfg!(debug_assertions) {
                panic!()
            }

            return None;
        }
    }
}

#[macro_export]
macro_rules! expr_error_log {
    ($toks:expr, $start:expr, $end:expr, $($arg:tt)*) => {
        {
            use cx_util::log_error;

            eprintln!("{}", $toks[$start .. $end].iter().map(|tok| format!("{}", tok)).collect::<Vec<_>>().join(" "));
            log_error!($($arg)*);
        }
    }
}

#[macro_export]
macro_rules! bytecode_error_log {
    ($builder:ident, $($arg:tt)*) => {
        {
            use cx_parsing_data::parse::macros::error_pointer;
            use cx_util::log_error;

            eprintln!("Error in method {}", $builder.current_function_name().unwrap_or("<unknown>"));
            log_error!($($arg)*);
        }
    }
}

#[macro_export]
macro_rules! point_log_error {
    ($toks:expr, $($arg:tt)*) => {
        {
            use cx_parsing_data::parse::macros::error_pointer;
            use cx_util::log_error;

            eprintln!("{}", error_pointer(&$toks));
            log_error!($($arg)*);
        }
    }
}

#[macro_export]
macro_rules! try_routine {
    ($iter:ident, $routine:expr) => {
        let iter_save = $iter.clone();

        if let Some(result) = $routine {
            result
        } else {
            *$iter = iter_save;
            None
        }
    };
}
