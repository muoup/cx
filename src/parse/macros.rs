#[macro_export]
macro_rules! log_error {
    ($($arg:tt)*) => {
        // If in debug mode, panic on error
        if cfg!(debug_assertions) {
            panic!($($arg)*);
        } else {
            eprintln!($($arg)*);
            return None;
        }
    }
}

#[macro_export]
macro_rules! assert_token_matches {
    ($toks:ident, $pattern:pat) => {
        let $pattern = $toks.next()? else {
            $toks.back();
            log_error!("Expected token to match pattern: {:#?}\n Found: {:#?}", stringify!($pattern), $toks.peek());
            return None;
        };
    }
}

#[macro_export]
macro_rules! try_token_matches {
    ($toks:ident, $pattern:pat) => {
        let $pattern = $toks.next()? else {
            $toks.back();
            return None;
        };
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