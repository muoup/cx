#[macro_export]
macro_rules! assert_token_matches {
    ($data:expr, $pattern:pat) => {
        assert_token_matches!($data, $pattern, stringify!($pattern));
    };

    ($data:expr, $pattern:pat, $expected:expr) => {
        let token_index = $data.index;
        let Some($pattern) = &$data.next().map(|t| &t.kind) else {
            $data.index = token_index;

            return $crate::log::parse_point_error(
                &$data,
                &$crate::log::EXPECTED_TOKEN,
                ($expected.to_string(), $data.peek().map(ToString::to_string)),
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
    ($data:expr) => {{
        match $data.next().map(|k| &k.kind) {
            Some(tok) => Ok(tok),
            None => $crate::log::parse_point_error(&$data, &$crate::log::UNEXPECTED_END_TOKENS, ()),
        }
    }};
}

#[macro_export]
macro_rules! peek_next_kind {
    ($data:expr) => {
        match $data.peek().map(|k| &k.kind) {
            Some(tok) => Ok(tok),
            None => $crate::log::parse_point_error(&$data, &$crate::log::UNEXPECTED_END_TOKENS, ()),
        }
    };
}