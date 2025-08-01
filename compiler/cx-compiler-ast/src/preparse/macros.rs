#[macro_export]
macro_rules! skip_token {
    ($tokens:expr, $token:pat) => {
        if matches!($token, $data.toks.peek()?.kind) {
            $tokens.next();
        }
    };
}