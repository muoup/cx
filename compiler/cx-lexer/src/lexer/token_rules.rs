use cx_log::CXResult;
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
        Some('\'') => char_literal(iter).map(Some),
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
                Some(TokenKind::Operator(OperatorType::Access))
            }
            Some('-') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::Decrement))
            }
            _ => try_assignment(iter, OperatorType::Minus),
        },
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
            _ => Some(TokenKind::Operator(OperatorType::Greater)),
        },
        '<' => match iter.peek() {
            Some('=') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::LessEqual))
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
    let start_iter = iter.cursor();
    while let Some(c) = iter.next() {
        if c == '\\' {
            iter.next();
        }

        if c == '"' {
            break;
        }
    }
    let string = iter.source()[start_iter..iter.cursor() - 1]
        .replace("\\n", "\n")
        .replace("\\t", "\t")
        .replace("\\r", "\r")
        .replace("\\\"", "\"");

    Some(TokenKind::StringLiteral(string))
}

fn char_literal(iter: &mut LexCursor<'_>) -> CXResult<TokenKind> {
    let start_index = iter.cursor();
    assert_eq!(iter.next(), Some('\''));

    let Some(c) = iter.next() else {
        return iter.log_error(start_index, "Unterminated character literal");
    };

    let Some(kind) = (match iter.next() {
        Some('\'') => Some(TokenKind::IntLiteral(IntegerLiteral::decimal(c as u64))),
        Some('0') if c == '\\' && iter.next() == Some('\'') => {
            Some(TokenKind::IntLiteral(IntegerLiteral::decimal(0)))
        }
        Some('n') if c == '\\' && iter.next() == Some('\'') => {
            Some(TokenKind::IntLiteral(IntegerLiteral::decimal('\n' as u64)))
        }
        Some('t') if c == '\\' && iter.next() == Some('\'') => {
            Some(TokenKind::IntLiteral(IntegerLiteral::decimal('\t' as u64)))
        }
        Some('r') if c == '\\' && iter.next() == Some('\'') => {
            Some(TokenKind::IntLiteral(IntegerLiteral::decimal('\r' as u64)))
        }
        _ => None,
    }) else {
        return iter.log_error(start_index, "Invalid character literal");
    };

    Ok(kind)
}
