#[macro_export]
macro_rules! log_error {
    ($($arg:tt)*) => {
        {
            use cx_util::CXError;
            let msg = format!($($arg)*);
            eprintln!("Error: {}", msg);

            panic!()
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
macro_rules! lmir_error_log {
    ($builder:ident, $($arg:tt)*) => {
        {
            use cx_util::log_error;

            eprintln!("Error in method {}", $builder.current_function_name().unwrap_or("<unknown>"));
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
