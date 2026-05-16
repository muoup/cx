use cx_tokens::token::TokenKind;
use cx_util::{CXResult, char_iter::CharIter};

pub(crate) fn number(iter: &mut CharIter, file_origin: &str) -> CXResult<TokenKind> {
    let start_index = iter.current_iter;

    if iter.peek() == Some('.') {
        iter.next();
        while matches!(iter.peek(), Some('0'..='9')) {
            iter.next();
        }

        if matches!(iter.peek(), Some('e' | 'E')) && !consume_exponent(iter) {
            consume_numeric_tail(iter);
            return invalid_numeric_literal(iter, file_origin, start_index);
        }

        return parse_float_literal(iter, file_origin, start_index);
    }

    if iter.peek() == Some('0') {
        iter.next();
        match iter.peek() {
            Some('x' | 'X') => {
                iter.next();
                return integer_with_radix(iter, file_origin, start_index, 16);
            }
            Some('b' | 'B') => {
                iter.next();
                return integer_with_radix(iter, file_origin, start_index, 2);
            }
            Some('.' | 'e' | 'E') => {}
            _ => {
                iter.back();
                return integer_with_radix(iter, file_origin, start_index, 8);
            }
        }
    }

    while matches!(iter.peek(), Some('0'..='9')) {
        iter.next();
    }

    let mut is_float = false;
    if iter.peek() == Some('.') {
        is_float = true;
        iter.next();
        while matches!(iter.peek(), Some('0'..='9')) {
            iter.next();
        }
    }

    if matches!(iter.peek(), Some('e' | 'E')) {
        is_float = true;
        if !consume_exponent(iter) {
            consume_numeric_tail(iter);
            return invalid_numeric_literal(iter, file_origin, start_index);
        }
    }

    if is_float {
        parse_float_literal(iter, file_origin, start_index)
    } else {
        let number_end = iter.current_iter;
        consume_integer_suffix(iter);
        if is_identifier_continue(iter.peek()) {
            consume_numeric_tail(iter);
            return invalid_numeric_literal(iter, file_origin, start_index);
        }

        parse_integer_literal(iter, file_origin, start_index, number_end, 10)
    }
}

fn integer_with_radix(
    iter: &mut CharIter,
    file_origin: &str,
    start_index: usize,
    radix: u32,
) -> CXResult<TokenKind> {
    let digit_start = iter.current_iter;
    while iter.peek().is_some_and(|c| c.is_digit(radix)) {
        iter.next();
    }

    let number_end = iter.current_iter;
    if digit_start == number_end {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, file_origin, start_index);
    }

    consume_integer_suffix(iter);
    if is_identifier_continue(iter.peek()) {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, file_origin, start_index);
    }

    parse_integer_literal(iter, file_origin, digit_start, number_end, radix)
}

fn parse_float_literal(
    iter: &mut CharIter,
    file_origin: &str,
    start_index: usize,
) -> CXResult<TokenKind> {
    let number_end = iter.current_iter;
    let bytes = consume_float_suffix(iter);
    if is_identifier_continue(iter.peek()) {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, file_origin, start_index);
    }

    let num = &iter.source[start_index..number_end];
    match num.parse() {
        Ok(value) => Ok(TokenKind::FloatLiteral(value, bytes)),
        Err(_) => log_lexer_error!(
            file_origin,
            iter.source,
            start_index,
            iter.current_iter,
            "Invalid numeric literal: {num}"
        ),
    }
}

fn parse_integer_literal(
    iter: &CharIter,
    file_origin: &str,
    digits_start: usize,
    digits_end: usize,
    radix: u32,
) -> CXResult<TokenKind> {
    let digits = &iter.source[digits_start..digits_end];
    match u64::from_str_radix(digits, radix) {
        Ok(value) => Ok(TokenKind::IntLiteral(value as i64)),
        Err(_) => log_lexer_error!(
            file_origin,
            iter.source,
            digits_start,
            iter.current_iter,
            "Invalid numeric literal: {}",
            &iter.source[digits_start..iter.current_iter]
        ),
    }
}

fn consume_exponent(iter: &mut CharIter) -> bool {
    iter.next();
    if matches!(iter.peek(), Some('+' | '-')) {
        iter.next();
    }

    let digit_start = iter.current_iter;
    while matches!(iter.peek(), Some('0'..='9')) {
        iter.next();
    }

    digit_start != iter.current_iter
}

fn consume_float_suffix(iter: &mut CharIter) -> u8 {
    match iter.peek() {
        Some('f' | 'F') => {
            iter.next();
            4
        }
        Some('l' | 'L') => {
            iter.next();
            8
        }
        _ => 8,
    }
}

fn consume_integer_suffix(iter: &mut CharIter) {
    let mut saw_unsigned = false;
    let mut saw_long = false;

    loop {
        match iter.peek() {
            Some('u' | 'U') if !saw_unsigned => {
                saw_unsigned = true;
                iter.next();
            }
            Some('l' | 'L') if !saw_long => {
                saw_long = true;
                let first = iter.next();
                if iter.peek() == first {
                    iter.next();
                }
            }
            _ => break,
        }
    }
}

fn consume_numeric_tail(iter: &mut CharIter) {
    while is_identifier_continue(iter.peek()) {
        iter.next();
    }
}

fn invalid_numeric_literal(
    iter: &CharIter,
    file_origin: &str,
    start_index: usize,
) -> CXResult<TokenKind> {
    log_lexer_error!(
        file_origin,
        iter.source,
        start_index,
        iter.current_iter,
        "Invalid numeric literal: {}",
        &iter.source[start_index..iter.current_iter]
    )
}

fn is_identifier_continue(c: Option<char>) -> bool {
    c.is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
}
