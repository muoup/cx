#[macro_export]
macro_rules! log_error {
    ($($arg:tt)*) => {
        {
            eprintln!("Error in file {} at line {}: ", file!(), line!());
            eprintln!($($arg)*);

            // If in debug mode, panic on error
            // if cfg!(debug_assertions) {
            //     panic!()
            // }

            return None;
        }
    }
}

#[macro_export]
macro_rules! point_log_error {
    ($data:ident, $($arg:tt)*) => {
        {
            use cx_util::log_error;
            use cx_util::error_pointer;

            eprintln!("{}", cx_util::error_pointer(&$data.toks));
            cx_util::macros::log_error!($($arg)*);
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
    }
}