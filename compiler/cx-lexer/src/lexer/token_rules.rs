use cx_log::CXResult;
use cx_log::catalogue::parse::{INVALID_LITERAL, UNEXPECTED_END};
use cx_tokens::{
    punctuator,
    token::{IntegerLiteral, OperatorType, PunctuatorType, TokenKind},
};

use crate::lexer::{number::number, source::LexCursor};

pub(crate) fn literal_or_prefixed_token(iter: &mut LexCursor<'_>) -> CXResult<Option<TokenKind>> {
    match iter.peek() {
        Some('0'..='9') => number(iter).map(Some),
        Some('.') if iter.next_is(u8::is_ascii_digit) => number(iter).map(Some),
        Some('"') => Ok(string(iter)),
        Some('\'') if !starts_lifetime_modifier(iter) => char_literal(iter).map(Some),
        _ => Ok(None),
    }
}

pub(crate) fn operator(iter: &mut LexCursor<'_>) -> Option<TokenKind> {
    fn try_assignment(iter: &mut LexCursor<'_>, operator: OperatorType) -> Option<TokenKind> {
        if Some('=') == iter.peek() {
            iter.next();
            Some(TokenKind::Assignment(Some(operator)))
        } else {
            Some(TokenKind::Operator(operator))
        }
    }

    match iter.next()? {
        '*' => try_assignment(iter, OperatorType::Asterisk),
        '/' => match iter.peek() {
            Some('/') => unreachable!("single-line comments are stripped before tokenization"),
            Some('*') => unreachable!("multi-line comments are stripped before tokenization"),
            _ => try_assignment(iter, OperatorType::Slash),
        },
        '%' => try_assignment(iter, OperatorType::Percent),
        '^' => try_assignment(iter, OperatorType::Caret),

        '|' => match iter.peek() {
            Some('|') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::DoubleBar))
            }
            Some('>') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::Pipe))
            }
            _ => try_assignment(iter, OperatorType::Bar),
        },

        '+' => match iter.peek() {
            Some('+') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::Increment))
            }
            _ => try_assignment(iter, OperatorType::Plus),
        },
        '-' => match iter.peek() {
            Some('>') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::Arrow))
            }
            Some('-') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::Decrement))
            }
            _ => try_assignment(iter, OperatorType::Minus),
        },
        '\'' => Some(TokenKind::Punctuator(PunctuatorType::Apostrophe)),
        '&' => match iter.peek() {
            Some('&') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::DoubleAmpersand))
            }
            _ => try_assignment(iter, OperatorType::Ampersand),
        },

        '.' => {
            if iter.next() == Some('.') && iter.peek() == Some('.') {
                iter.next();
                Some(TokenKind::Punctuator(PunctuatorType::Ellipsis))
            } else {
                iter.back();
                Some(TokenKind::Operator(OperatorType::Access))
            }
        }
        '!' => {
            if Some('=') == iter.peek() {
                iter.next();
                Some(TokenKind::Operator(OperatorType::NotEqual))
            } else {
                Some(TokenKind::Operator(OperatorType::Exclamation))
            }
        }
        '~' => Some(TokenKind::Operator(OperatorType::Tilda)),

        ':' if Some(':') == iter.peek() => {
            iter.next();
            Some(TokenKind::Operator(OperatorType::ScopeRes))
        }

        '>' => match iter.peek() {
            Some('=') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::GreaterEqual))
            }
            Some('>') => {
                iter.next();
                if iter.peek() == Some('=') {
                    iter.next();
                    Some(TokenKind::Assignment(Some(OperatorType::RShift)))
                } else {
                    Some(TokenKind::Operator(OperatorType::RShift))
                }
            }
            _ => Some(TokenKind::Operator(OperatorType::Greater)),
        },
        '<' => match iter.peek() {
            Some('|') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::BackwardPipe))
            }
            Some('=') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::LessEqual))
            }
            Some('<') => {
                iter.next();
                if iter.peek() == Some('=') {
                    iter.next();
                    Some(TokenKind::Assignment(Some(OperatorType::LShift)))
                } else {
                    Some(TokenKind::Operator(OperatorType::LShift))
                }
            }
            _ => Some(TokenKind::Operator(OperatorType::Less)),
        },
        '=' => match iter.peek() {
            Some('=') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::Equal))
            }
            Some('>') => {
                iter.next();
                Some(punctuator!(ThickArrow))
            }
            _ => Some(TokenKind::Assignment(None)),
        },
        ',' => Some(TokenKind::Operator(OperatorType::Comma)),
        _ => {
            iter.back();
            None
        }
    }
}

pub(crate) fn punctuator(iter: &mut LexCursor<'_>) -> Option<TokenKind> {
    if !iter.has_next() {
        return None;
    }

    match iter.next().unwrap() {
        '(' => Some(TokenKind::Punctuator(PunctuatorType::OpenParen)),
        ')' => Some(TokenKind::Punctuator(PunctuatorType::CloseParen)),
        '[' => Some(TokenKind::Punctuator(PunctuatorType::OpenBracket)),
        ']' => Some(TokenKind::Punctuator(PunctuatorType::CloseBracket)),
        '{' => Some(TokenKind::Punctuator(PunctuatorType::OpenBrace)),
        '}' => Some(TokenKind::Punctuator(PunctuatorType::CloseBrace)),
        ';' => Some(TokenKind::Punctuator(PunctuatorType::Semicolon)),
        ':' => Some(TokenKind::Punctuator(PunctuatorType::Colon)),
        '.' => Some(TokenKind::Punctuator(PunctuatorType::Period)),
        '?' => Some(TokenKind::Punctuator(PunctuatorType::QuestionMark)),
        '#' => Some(TokenKind::Punctuator(PunctuatorType::Hash)),
        _ => {
            iter.back();
            None
        }
    }
}

fn string(iter: &mut LexCursor<'_>) -> Option<TokenKind> {
    assert_eq!(iter.next(), Some('"'));
    let mut string = String::new();
    while let Some(c) = iter.next() {
        match c {
            '"' => break,
            '\\' => match escape_sequence(iter) {
                Some(value) => string.push(char::from(value)),
                None => {
                    string.push('\\');
                    string.extend(iter.next());
                }
            },
            _ => string.push(c),
        }
    }

    Some(TokenKind::StringLiteral(string))
}

fn escape_sequence(iter: &mut LexCursor<'_>) -> Option<u8> {
    let simple = match iter.peek()? {
        'n' => b'\n',
        't' => b'\t',
        'r' => b'\r',
        'a' => 0x07,
        'b' => 0x08,
        'f' => 0x0c,
        'v' => 0x0b,
        'e' => 0x1b,
        '\\' => b'\\',
        '\'' => b'\'',
        '"' => b'"',
        '?' => b'?',
        '0'..='7' => return Some(radix_escape(iter, 8, 3)),
        'x' if iter.next_is(u8::is_ascii_hexdigit) => {
            iter.next();
            return Some(radix_escape(iter, 16, usize::MAX));
        }
        _ => return None,
    };

    iter.next();
    Some(simple)
}

fn radix_escape(iter: &mut LexCursor<'_>, radix: u32, max_digits: usize) -> u8 {
    let mut value = 0u32;
    let mut digits = 0;
    while digits < max_digits
        && let Some(digit) = iter.peek().and_then(|c| c.to_digit(radix))
    {
        value = value.wrapping_mul(radix).wrapping_add(digit);
        digits += 1;
        iter.next();
    }

    value as u8
}

fn char_literal(iter: &mut LexCursor<'_>) -> CXResult<TokenKind> {
    let start_index = iter.cursor();
    assert_eq!(iter.next(), Some('\''));

    let value = match iter.next() {
        Some('\\') => escape_sequence(iter).map(u64::from),
        Some('\'') => None,
        Some(c) => Some(c as u64),
        None => {
            return iter.log_error(start_index, &UNEXPECTED_END, Some("character literal".into()));
        }
    };

    match value {
        Some(value) if iter.next() == Some('\'') => {
            Ok(TokenKind::IntLiteral(IntegerLiteral::decimal(value)))
        }
        _ => iter.log_error(start_index, &INVALID_LITERAL, ("character".into(), None)),
    }
}

pub(crate) fn starts_lifetime_modifier(iter: &LexCursor<'_>) -> bool {
    let source = &iter.source()[iter.cursor()..];
    let mut chars = source.chars().peekable();
    if chars.next() != Some('\'') {
        return false;
    }

    let Some(first) = chars.next() else {
        return false;
    };

    if first != '_' && !first.is_ascii_alphabetic() {
        return false;
    }

    if first != '_' && !first.is_ascii_alphabetic() {
        return false;
    }

    while chars
        .peek()
        .is_some_and(|character| *character == '_' || character.is_ascii_alphanumeric())
    {
        chars.next();
    }

    chars.peek() != Some(&'\'')
}
