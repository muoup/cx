#[macro_export]
macro_rules! assert_token_matches {
    ($toks:ident, $pattern:pat) => {
        let $pattern = $toks.next()? else {
            $toks.back();
            println!("Expected token to match pattern: {:?}", stringify!($pattern));
            println!("Found: {:?}", $toks.peek());
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