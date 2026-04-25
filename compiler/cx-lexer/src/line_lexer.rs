use std::sync::Arc;

use cx_tokens::punctuator;
use cx_tokens::token::{OperatorType, PunctuatorType, Token, TokenKind};
use cx_util::{CXResult, char_iter::CharIter};

pub(crate) struct LineLexer<'a> {
    last_consume: usize,
    iter: &'a mut CharIter<'a>,
    file_origin: Arc<str>,

    pub tokens: Vec<Token>,
}

pub(crate) fn lex_line<'a>(
    iter: &'a mut CharIter<'a>,
    file_origin: String,
) -> CXResult<Vec<Token>> {
    let mut line_lexer = LineLexer::new(iter, file_origin);
    line_lexer.generate_tokens()?;
    Ok(line_lexer.tokens)
}

impl<'a> LineLexer<'a> {
    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,

            line: self.iter.line,
            start_index: self.last_consume,
            end_index: self.iter.current_iter,
            file_origin: self.file_origin.clone(),
        })
    }

    pub(crate) fn generate_tokens(&mut self) -> CXResult<()> {
        while self.iter.has_next() && self.iter.peek() != Some('\n') {
            if self.last_consume == self.iter.current_iter
                && let Some(token) = self.pre_ident_lex()?
            {
                self.add_token(token);
                self.last_consume = self.iter.current_iter;
            }

            let previous_lex = self.iter.current_iter;

            if let Some(operator) = operator_lex(self.iter).or_else(|| punctuator_lex(self.iter)) {
                self.consume(previous_lex);

                self.add_token(operator);
                self.last_consume = self.iter.current_iter;
            } else if Some(true) == self.iter.peek().map(|c| c.is_whitespace()) {
                self.consume(previous_lex);

                while let Some(true) = self.iter.peek().map(|c| c.is_whitespace()) {
                    self.iter.next();
                }

                self.last_consume = self.iter.current_iter;
            } else {
                self.iter.next();
            }
        }

        self.consume(self.iter.current_iter);
        Ok(())
    }

    fn new(iter: &'a mut CharIter<'a>, file_origin: String) -> LineLexer<'a> {
        let last_consume = iter.current_iter;

        LineLexer {
            iter,
            last_consume,
            tokens: Vec::new(),
            file_origin: Arc::from(file_origin),
        }
    }

    fn consume(&mut self, up_to: usize) {
        if up_to == self.last_consume {
            return;
        }

        let str = self.iter.source[self.last_consume..up_to].to_string();
        if str.chars().any(|c| !c.is_whitespace()) {
            self.add_token(TokenKind::from_str(str));
        }

        self.last_consume = self.iter.current_iter;
    }

    fn pre_ident_lex(&mut self) -> CXResult<Option<TokenKind>> {
        match self.iter.peek() {
            Some('0'..='9') => number_lex(self.iter, self.file_origin.as_ref()).map(Some),
            Some('"') => Ok(string_lex(self.iter)),
            Some('\'') => char_lex(self.iter, self.file_origin.as_ref()).map(Some),
            _ => Ok(None),
        }
    }
}

fn number_lex(iter: &mut CharIter, file_origin: &str) -> CXResult<TokenKind> {
    let start_index = iter.current_iter;
    let mut dot = false;
    while let Some(c) = iter.peek() {
        if c == '.' {
            dot = true;
        } else if !c.is_ascii_digit() {
            break;
        }
        iter.next();
    }
    let number_end = iter.current_iter;

    while let Some(c) = iter.peek() {
        if matches!(c, 'u' | 'U' | 'l' | 'L') {
            iter.next();
        } else {
            break;
        }
    }

    let num = &iter.source[start_index..number_end];

    if dot {
        match num.parse() {
            Ok(value) => Ok(TokenKind::FloatLiteral(value)),
            Err(_) => log_lexer_error!(
                file_origin,
                iter.source,
                start_index,
                iter.current_iter,
                "Invalid numeric literal: {num}"
            ),
        }
    } else {
        match num.parse() {
            Ok(value) => Ok(TokenKind::IntLiteral(value)),
            Err(_) => log_lexer_error!(
                file_origin,
                iter.source,
                start_index,
                iter.current_iter,
                "Invalid numeric literal: {num}"
            ),
        }
    }
}

fn string_lex(iter: &mut CharIter) -> Option<TokenKind> {
    assert_eq!(iter.next(), Some('"'));
    let start_iter = iter.current_iter;
    while let Some(c) = iter.next() {
        if c == '\\' {
            // skip the next character
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

fn char_lex(iter: &mut CharIter, file_origin: &str) -> CXResult<TokenKind> {
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

fn operator_lex(iter: &mut CharIter) -> Option<TokenKind> {
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
            Some('/') => {
                iter.next();
                while let Some(c) = iter.next() {
                    if c == '\n' {
                        break;
                    }
                }
                panic!("Single line comments should not reach the operator lexer")
            }
            Some('*') => {
                iter.next();
                while let Some(c) = iter.next() {
                    if c == '*' && iter.peek() == Some('/') {
                        iter.next();
                        break;
                    }
                }
                panic!("Multi line comments should not reach the operator lexer")
            }
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

fn punctuator_lex(iter: &mut CharIter) -> Option<TokenKind> {
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
