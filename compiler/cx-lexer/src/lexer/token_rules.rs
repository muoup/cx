use cx_tokens::{
    punctuator,
    token::{OperatorType, PunctuatorType, TokenKind},
};
use cx_util::{CXResult, char_iter::CharIter};

use crate::lexer::number::number;

pub(crate) fn literal_or_prefixed_token(
    iter: &mut CharIter,
    file_origin: &str,
) -> CXResult<Option<TokenKind>> {
    match iter.peek() {
        Some('0'..='9') => number(iter, file_origin).map(Some),
        Some('"') => Ok(string(iter)),
        Some('\'') => char_literal(iter, file_origin).map(Some),
        _ => Ok(None),
    }
}

pub(crate) fn operator(iter: &mut CharIter) -> Option<TokenKind> {
    fn try_assignment(iter: &mut CharIter, operator: OperatorType) -> Option<TokenKind> {
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
        '.' => {
            if iter.next() == Some('.') && iter.peek() == Some('.') {
                iter.next();
                Some(TokenKind::Punctuator(PunctuatorType::Ellipsis))
            } else {
                iter.back();
                Some(TokenKind::Operator(OperatorType::Access))
            }
        }

        '|' => match iter.peek() {
            Some('|') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::DoubleBar))
            }
            _ => Some(TokenKind::Operator(OperatorType::Bar)),
        },
        '&' => match iter.peek() {
            Some('&') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::DoubleAmpersand))
            }
            _ => Some(TokenKind::Operator(OperatorType::Ampersand)),
        },
        '^' => Some(TokenKind::Operator(OperatorType::Caret)),
        '!' => {
            if Some('=') == iter.peek() {
                iter.next();
                Some(TokenKind::Operator(OperatorType::NotEqual))
            } else {
                Some(TokenKind::Operator(OperatorType::Exclamation))
            }
        }
        '~' => Some(TokenKind::Operator(OperatorType::Tilda)),

        ':' => {
            if Some(':') == iter.peek() {
                iter.next();
                Some(TokenKind::Operator(OperatorType::ScopeRes))
            } else {
                iter.back();
                None
            }
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

pub(crate) fn punctuator(iter: &mut CharIter) -> Option<TokenKind> {
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
        _ => {
            iter.back();
            None
        }
    }
}

fn string(iter: &mut CharIter) -> Option<TokenKind> {
    assert_eq!(iter.next(), Some('"'));
    let start_iter = iter.current_iter;
    while let Some(c) = iter.next() {
        if c == '\\' {
            iter.next();
        }

        if c == '"' {
            break;
        }
    }
    let string = iter.source[start_iter..iter.current_iter - 1]
        .replace("\\n", "\n")
        .replace("\\t", "\t")
        .replace("\\r", "\r")
        .replace("\\\"", "\"");

    Some(TokenKind::StringLiteral(string))
}

fn char_literal(iter: &mut CharIter, file_origin: &str) -> CXResult<TokenKind> {
    let start_index = iter.current_iter;
    assert_eq!(iter.next(), Some('\''));

    let Some(c) = iter.next() else {
        return log_lexer_error!(
            file_origin,
            iter.source,
            start_index,
            iter.current_iter,
            "Unterminated character literal"
        );
    };

    let Some(kind) = (match iter.next() {
        Some('\'') => Some(TokenKind::IntLiteral(c as i64)),
        Some('0') if c == '\\' && iter.next() == Some('\'') => Some(TokenKind::IntLiteral(0)),
        Some('n') if c == '\\' && iter.next() == Some('\'') => {
            Some(TokenKind::IntLiteral('\n' as i64))
        }
        Some('t') if c == '\\' && iter.next() == Some('\'') => {
            Some(TokenKind::IntLiteral('\t' as i64))
        }
        Some('r') if c == '\\' && iter.next() == Some('\'') => {
            Some(TokenKind::IntLiteral('\r' as i64))
        }
        _ => None,
    }) else {
        return log_lexer_error!(
            file_origin,
            iter.source,
            start_index,
            iter.current_iter,
            "Invalid character literal"
        );
    };

    Ok(kind)
}
