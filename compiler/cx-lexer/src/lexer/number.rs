use cx_log::CXResult;
use cx_tokens::token::{
    FloatLiteral, FloatSuffix, IntegerBase, IntegerLength, IntegerLiteral, IntegerSuffix, TokenKind,
};

use crate::lexer::source::LexCursor;

pub(crate) fn number(iter: &mut LexCursor<'_>) -> CXResult<TokenKind> {
    let start_index = iter.cursor();

    if iter.peek() == Some('.') {
        iter.next();
        while matches!(iter.peek(), Some('0'..='9')) {
            iter.next();
        }

        if matches!(iter.peek(), Some('e' | 'E')) && !consume_exponent(iter) {
            consume_numeric_tail(iter);
            return invalid_numeric_literal(iter, start_index);
        }

        return parse_float_literal(iter, start_index);
    }

    if iter.peek() == Some('0') {
        iter.next();
        match iter.peek() {
            Some('x' | 'X') => {
                iter.next();
                return hexadecimal_literal(iter, start_index);
            }
            Some('b' | 'B') => {
                iter.next();
                return integer_with_radix(iter, start_index, 2);
            }
            Some('.' | 'e' | 'E') => {}
            _ => {
                iter.back();
                return integer_with_radix(iter, start_index, 8);
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
            return invalid_numeric_literal(iter, start_index);
        }
    }

    if is_float {
        parse_float_literal(iter, start_index)
    } else {
        let number_end = iter.cursor();
        let suffix = consume_integer_suffix(iter);
        if is_identifier_continue(iter.peek()) {
            consume_numeric_tail(iter);
            return invalid_numeric_literal(iter, start_index);
        }

        parse_integer_literal(
            iter,
            start_index,
            number_end,
            10,
            IntegerBase::Decimal,
            suffix,
        )
    }
}

fn hexadecimal_literal(iter: &mut LexCursor<'_>, start_index: usize) -> CXResult<TokenKind> {
    let digit_start = iter.cursor();
    while iter.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
        iter.next();
    }

    if matches!(iter.peek(), Some('.') | Some('p' | 'P')) {
        return hexadecimal_float_literal(iter, start_index);
    }

    let number_end = iter.cursor();
    if digit_start == number_end {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, start_index);
    }

    let suffix = consume_integer_suffix(iter);
    if is_identifier_continue(iter.peek()) {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, start_index);
    }

    parse_integer_literal(
        iter,
        digit_start,
        number_end,
        16,
        IntegerBase::Hexadecimal,
        suffix,
    )
}

fn hexadecimal_float_literal(iter: &mut LexCursor<'_>, start_index: usize) -> CXResult<TokenKind> {
    if iter.peek() == Some('.') {
        iter.next();
        while iter.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
            iter.next();
        }
    }

    if !matches!(iter.peek(), Some('p' | 'P')) {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, start_index);
    }
    iter.next();

    if matches!(iter.peek(), Some('+' | '-')) {
        iter.next();
    }

    let exponent_start = iter.cursor();
    while matches!(iter.peek(), Some('0'..='9')) {
        iter.next();
    }
    if exponent_start == iter.cursor() {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, start_index);
    }

    let number_end = iter.cursor();
    let suffix = consume_float_suffix(iter);
    if is_identifier_continue(iter.peek()) {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, start_index);
    }

    let number = &iter.source()[start_index..number_end];
    let value = match parse_hexadecimal_float(number) {
        Ok(value) => value,
        Err(_) => return iter.log_error(start_index, format!("Invalid numeric literal: {number}")),
    };

    let value = match suffix {
        FloatSuffix::Float => value as f32 as f64,
        FloatSuffix::Default | FloatSuffix::LongDouble => value,
    };

    Ok(TokenKind::FloatLiteral(FloatLiteral { value, suffix }))
}

fn parse_hexadecimal_float(number: &str) -> Result<f64, ()> {
    let number = number
        .strip_prefix("0x")
        .or_else(|| number.strip_prefix("0X"))
        .ok_or(())?;
    let exponent_index = number.find(['p', 'P']).ok_or(())?;
    let (mantissa, exponent) = number.split_at(exponent_index);
    let exponent = exponent[1..].parse::<i32>().map_err(|_| ())?;

    let mut value = 0.0;
    let mut fraction = false;
    let mut divisor = 1.0;

    for digit in mantissa.chars() {
        if digit == '.' {
            if fraction {
                return Err(());
            }
            fraction = true;
            continue;
        }

        let digit = digit.to_digit(16).ok_or(())? as f64;
        if fraction {
            divisor *= 16.0;
            value += digit / divisor;
        } else {
            value = value * 16.0 + digit;
        }
    }

    Ok(value * 2.0f64.powi(exponent))
}

fn integer_with_radix(
    iter: &mut LexCursor<'_>,
    start_index: usize,
    radix: u32,
) -> CXResult<TokenKind> {
    let digit_start = iter.cursor();
    while iter.peek().is_some_and(|c| c.is_digit(radix)) {
        iter.next();
    }

    let number_end = iter.cursor();
    if digit_start == number_end {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, start_index);
    }

    let suffix = consume_integer_suffix(iter);
    if is_identifier_continue(iter.peek()) {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, start_index);
    }

    let base = match radix {
        2 => IntegerBase::Binary,
        8 => IntegerBase::Octal,
        16 => IntegerBase::Hexadecimal,
        _ => unreachable!(),
    };
    parse_integer_literal(iter, digit_start, number_end, radix, base, suffix)
}

fn parse_float_literal(iter: &mut LexCursor<'_>, start_index: usize) -> CXResult<TokenKind> {
    let number_end = iter.cursor();
    let suffix = consume_float_suffix(iter);
    if is_identifier_continue(iter.peek()) {
        consume_numeric_tail(iter);
        return invalid_numeric_literal(iter, start_index);
    }

    let num = &iter.source()[start_index..number_end];
    let value = match suffix {
        FloatSuffix::Float => num.parse::<f32>().map(|value| value as f64),
        FloatSuffix::Default | FloatSuffix::LongDouble => num.parse::<f64>(),
    };
    match value {
        Ok(value) => Ok(TokenKind::FloatLiteral(FloatLiteral { value, suffix })),
        Err(_) => iter.log_error(start_index, format!("Invalid numeric literal: {num}")),
    }
}

fn parse_integer_literal(
    iter: &LexCursor<'_>,
    digits_start: usize,
    digits_end: usize,
    radix: u32,
    base: IntegerBase,
    suffix: IntegerSuffix,
) -> CXResult<TokenKind> {
    let digits = &iter.source()[digits_start..digits_end];
    match u64::from_str_radix(digits, radix) {
        Ok(magnitude) => Ok(TokenKind::IntLiteral(IntegerLiteral {
            magnitude,
            base,
            suffix,
        })),
        Err(_) => iter.log_error(
            digits_start,
            format!(
                "Invalid numeric literal: {}",
                &iter.source()[digits_start..iter.cursor()]
            ),
        ),
    }
}

fn consume_exponent(iter: &mut LexCursor<'_>) -> bool {
    iter.next();
    if matches!(iter.peek(), Some('+' | '-')) {
        iter.next();
    }

    let digit_start = iter.cursor();
    while matches!(iter.peek(), Some('0'..='9')) {
        iter.next();
    }

    digit_start != iter.cursor()
}

fn consume_float_suffix(iter: &mut LexCursor<'_>) -> FloatSuffix {
    match iter.peek() {
        Some('f' | 'F') => {
            iter.next();
            FloatSuffix::Float
        }
        Some('l' | 'L') => {
            iter.next();
            FloatSuffix::LongDouble
        }
        _ => FloatSuffix::Default,
    }
}

fn consume_integer_suffix(iter: &mut LexCursor<'_>) -> IntegerSuffix {
    let mut saw_unsigned = false;
    let mut length = IntegerLength::Default;

    loop {
        match iter.peek() {
            Some('u' | 'U') if !saw_unsigned => {
                saw_unsigned = true;
                iter.next();
            }
            Some('l' | 'L') if length == IntegerLength::Default => {
                let first = iter.next();
                if iter.peek() == first {
                    iter.next();
                    length = IntegerLength::LongLong;
                } else {
                    length = IntegerLength::Long;
                }
            }
            _ => break,
        }
    }

    IntegerSuffix {
        unsigned: saw_unsigned,
        length,
    }
}

fn consume_numeric_tail(iter: &mut LexCursor<'_>) {
    while is_identifier_continue(iter.peek()) {
        iter.next();
    }
}

fn invalid_numeric_literal(iter: &LexCursor<'_>, start_index: usize) -> CXResult<TokenKind> {
    iter.log_error(
        start_index,
        format!(
            "Invalid numeric literal: {}",
            &iter.source()[start_index..iter.cursor()]
        ),
    )
}

fn is_identifier_continue(c: Option<char>) -> bool {
    c.is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
}
