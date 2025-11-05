use cx_lexer_data::TokenIter;

pub fn token_string(toks: &TokenIter, start_index: usize, end_index: usize) -> String {
    let mut tokens = String::new();

    for tok in &toks.slice[start_index..end_index] {
        tokens.push_str(&format!("{tok} "));
    }

    tokens.push('\n');

    tokens
}

#[macro_export]
macro_rules! assert_token_matches {
    ($data:expr, $pattern:pat) => {
        let Some($pattern) = &$data.next().map(|t| &t.kind) else {
            use cx_util::log_error;

            $data.back();

            return log_preparse_error!(
                $data,
                "Expected token to match pattern: {:#?}\n Found: {}",
                stringify!($pattern),
                $data.peek().unwrap()
            );
        };
    };
}

#[macro_export]
macro_rules! try_token_matches {
    ($data:ident, $pattern:pat) => {
        let Some($pattern) = $data.toks.peek() else {
            return None;
        };
        $data.toks.next();
    };
}

#[macro_export]
macro_rules! try_next {
    ($data:expr, $pattern:pat) => {
        if matches!($data.peek().map(|k| &k.kind), Some($pattern)) {
            $data.next();
            true
        } else {
            false
        }
    };
}

#[macro_export]
macro_rules! peek_kind {
    ($data:expr, $pattern:pat) => {
        matches!($data.peek().map(|k| &k.kind), Some($pattern))
    };
}

#[macro_export]
macro_rules! next_kind {
    ($data:expr) => {
        $data.next().cloned().map(|k| k.kind)
            .ok_or_else(|| cx_util::CXError::new("Unexpected end of tokens"))
    };
}

#[macro_export]
macro_rules! peek_next_kind {
    ($data:expr) => {
        $data.peek().map(|k| &k.kind)
            .ok_or_else(|| cx_util::CXError::new("Unexpected end of tokens"))
    };
}