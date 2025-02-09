#[macro_export]
macro_rules! assert_token_matches {
    ($pattern:pat) => {
        let $pattern = toks.next()? else {
            toks.back();
            warn!("Expected token to match pattern: {:?}", stringify!($pattern));
            warn!("Found: {:?}", toks.peek());
            return None;
        };
    }
}

#[macro_export]
macro_rules! try_token_matches {
    ($pattern:pat) => {
        let $pattern = toks.next()? else {
            toks.back();
            return None;
        };
    }
}