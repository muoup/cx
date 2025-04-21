#[macro_export]
macro_rules! log_error {
    ($($arg:tt)*) => {
        // If in debug mode, panic on error
        if cfg!(debug_assertions) {
            eprintln!("Error in file {} at line {}: ", file!(), line!());
            panic!($($arg)*);
        } else {
            eprintln!($($arg)*);
            return None;
        }
    }
}

#[macro_export]
macro_rules! assert_token_matches {
    ($data:ident, $pattern:pat) => {
        let $pattern = $data.toks.next()? else {
            $data.toks.back();
            log_error!("Expected token to match pattern: {:#?}\n Found: {:#?}", stringify!($pattern), $data.toks.peek());
            return None;
        };
    }
}

#[macro_export]
macro_rules! try_token_matches {
    ($data:ident, $pattern:pat) => {
        let Some($pattern) = $data.toks.peek() else {
            return None;
        };
        $data.toks.next();
    }
}

#[macro_export]
macro_rules! tok_next {
    ($data:ident, $pattern:pat) => {
        if let Some($pattern) = $data.toks.peek() {
            $data.toks.next();
            true
        } else {
            false
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