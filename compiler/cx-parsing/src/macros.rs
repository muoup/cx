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

#[cfg(test)]
mod tests {
    use cx_log::CXResult;
    use cx_tokens::{
        token::{Token, TokenKind},
        TokenIter,
    };

    fn expect_identifier(tokens: &mut TokenIter<'_>) -> CXResult<()> {
        assert_token_matches!(*tokens, TokenKind::Identifier(_), "identifier");
        Ok(())
    }

    fn next(tokens: &mut TokenIter<'_>) -> CXResult<()> {
        next_kind!(*tokens)?;
        Ok(())
    }

    fn peek(tokens: &mut TokenIter<'_>) -> CXResult<()> {
        peek_next_kind!(*tokens)?;
        Ok(())
    }

    #[test]
    fn empty_input_returns_diagnostics_without_rewinding() {
        let mut tokens = TokenIter::new(&[], "empty.cx".into());
        let error = expect_identifier(&mut tokens).unwrap_err();
        assert_eq!(error.code(), "P0072");
        assert!(error.message().contains("end of input"));
        assert_eq!(tokens.index, 0);
        assert_eq!(next(&mut tokens).unwrap_err().code(), "P0073");
        assert_eq!(peek(&mut tokens).unwrap_err().code(), "P0073");
        assert_eq!(tokens.index, 0);
    }

    #[test]
    fn exhausted_input_does_not_report_the_previous_token_as_found() {
        let source = [Token::new_unknown(TokenKind::Identifier("name".into()))];
        let mut tokens = TokenIter::new(&source, "example.cx".into());
        tokens.next();
        let error = expect_identifier(&mut tokens).unwrap_err();
        assert!(error.message().contains("end of input"));
        assert_eq!(tokens.index, 1);
    }
}
