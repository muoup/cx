use std::io::BufRead;
use cx_data_ast::lex::token::{OperatorType, PunctuatorType, TokenKind, Token};
use cx_util::char_iter::CharIter;

pub(crate) struct Lexer<'a> {
    source: &'a str,

    last_consume: usize,
    iter: CharIter<'a>,

    pub tokens: Vec<Token>,
}

impl Lexer<'_> {
    pub(crate) fn add_token(&mut self, token: Token) {
        if !matches!(token.kind, TokenKind::Ignore) {
            self.tokens.push(token);
        }
    }
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source,

            last_consume: 0,
            iter: CharIter {
                source,
                
                current_iter: 0,
                line: 1,
            },

            tokens: Vec::new(),
        }
    }

    pub fn generate_tokens(&mut self) {
        while self.iter.has_next() {
            if self.last_consume == self.iter.current_iter {
                if let Some(token) = self.pre_ident_lex() {
                    self.tokens.push(token);
                    self.last_consume = self.iter.current_iter;
                }
            }

            let previous_lex = self.iter.current_iter;

            if let Some(operator) = operator_lex(&mut self.iter)
                .or_else(|| punctuator_lex(&mut self.iter)) {
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
    }

    fn consume(&mut self, up_to: usize) {
        if up_to == self.last_consume {
            return;
        }

        let str = self.source[self.last_consume.. up_to].to_string();
        self.last_consume = self.iter.current_iter;

        if str.chars().any(|c| !c.is_whitespace()) {
            let kind = TokenKind::from_str(str);
            
            self.add_token(
                Token {
                    kind,
                    line: self.iter.line,
                    start_index: self.last_consume,
                    end_index: up_to,
                }
            )
        }
    }

    fn pre_ident_lex(&mut self) -> Option<Token> {
        match self.iter.peek()? {
            '0' .. '9' => number_lex(&mut self.iter),
            '"' => string_lex(&mut self.iter),
            '\'' => char_lex(&mut self.iter),
            _ => None
        }
    }
}

fn number_lex(iter: &mut CharIter) -> Option<Token> {
    let start_index = iter.current_iter;
    let mut dot = false;
    while let Some(c) = iter.peek() {
        if c == '.' {
            dot = true;
        } else if !c.is_digit(10) {
            break;
        }
        iter.next();
    }
    let num = &iter.source[start_index..iter.current_iter];
    let kind = if dot {
        TokenKind::FloatLiteral(num.parse().unwrap())
    } else {
        TokenKind::IntLiteral(num.parse().expect(&format!("Invalid number: {}\n", num)))
    };
    
    Some(
        Token {
            kind,
            line: iter.line,
            start_index,
            end_index: iter.current_iter,
        }
    )
}

fn string_lex(iter: &mut CharIter) -> Option<Token> {
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
    let string =
        iter.source[start_iter..iter.current_iter - 1]
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\\"", "\"");

    Some(
        Token {
            kind: TokenKind::StringLiteral(string),
        
            line: iter.line,
            start_index: start_iter,
            end_index: iter.current_iter - 1,
        }
    )
}

fn char_lex(iter: &mut CharIter) -> Option<Token> {
    assert_eq!(iter.next(), Some('\''));
    let start_index = iter.current_iter;
    let c = iter.next()?;

    let kind = match iter.next()? {
        '\'' => TokenKind::IntLiteral(c as i64),
        '0' => {
            assert_eq!(c, '\\');
            assert_eq!(iter.next(), Some('\''));
            TokenKind::IntLiteral(0)
        },
        'n' => {
            assert_eq!(c, '\\');
            assert_eq!(iter.next(), Some('\''));
            TokenKind::IntLiteral('\n' as i64)
        },
        't' => {
            assert_eq!(c, '\\');
            assert_eq!(iter.next(), Some('\''));
            TokenKind::IntLiteral('\t' as i64)
        },
        'r' => {
            assert_eq!(c, '\\');
            assert_eq!(iter.next(), Some('\''));
            TokenKind::IntLiteral('\r' as i64)
        },
        _ => panic!("Invalid character literal: '{}'", c)
    };
    
    Some(
        Token {
            kind,
            line: iter.line,
            start_index,
            end_index: iter.current_iter,
        }
    )
}

fn operator_lex(iter: &mut CharIter) -> Option<Token> {
    fn try_assignment(iter: &mut CharIter, operator: OperatorType) -> Option<TokenKind> {
        if Some('=') == iter.peek() {
            iter.next();
            Some(TokenKind::Assignment(Some(operator)))
        } else {
            Some(TokenKind::Operator(operator))
        }
    }
    
    let start_index = iter.current_iter;

    let kind = match iter.next()? {
        '*' => try_assignment(iter, OperatorType::Asterisk),
        '/' => {
            match iter.peek() {
                Some('/') => {
                    iter.next();
                    while let Some(c) = iter.next() {
                        if c == '\n' {
                            break;
                        }
                    }
                    Some(TokenKind::Ignore)
                },
                Some('*') => {
                    iter.next();
                    while let Some(c) = iter.next() {
                        if c == '*' && iter.peek() == Some('/') {
                            iter.next();
                            break;
                        }
                    }
                    Some(TokenKind::Ignore)
                },
                _ => try_assignment(iter, OperatorType::Slash)
            }
        },
        '%' => try_assignment(iter, OperatorType::Percent),

        '+' => match iter.peek() {
            Some('+') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::Increment))
            },
            _ => try_assignment(iter, OperatorType::Plus)
        }
        '-' => match iter.peek() {
            Some('>') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::Access))
            },
            Some('-') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::Decrement))
            },
            _ => try_assignment(iter, OperatorType::Minus)
        },
        '.' => {
            if iter.next() == Some('.') && iter.peek() == Some('.') {
                iter.next();
                Some(TokenKind::Punctuator(PunctuatorType::Ellipsis))
            } else {
                iter.back();
                Some(TokenKind::Operator(OperatorType::Access))
            }
        },

        '|' => match iter.peek() {
            Some('|') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::LOr))
            },
            _ => Some(TokenKind::Operator(OperatorType::BOr))
        },
        '&' => match iter.peek() {
            Some('&') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::LAnd))
            },
            _ => Some(TokenKind::Operator(OperatorType::BAnd))
        },
        '^' => Some(TokenKind::Operator(OperatorType::BXor)),
        '!' => {
            if Some('=') == iter.peek() {
                iter.next();
                Some(TokenKind::Operator(OperatorType::NotEqual))
            } else {
                Some(TokenKind::Operator(OperatorType::LNot))
            }
        },
        '~' => Some(TokenKind::Operator(OperatorType::BNot)),

        ':' => {
            if Some(':') == iter.peek() {
                iter.next();
                Some(TokenKind::Operator(OperatorType::ScopeRes))
            } else {
                iter.back();
                None
            }
        },

        '>' => match iter.peek() {
            Some('>') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::RShift))
            },
            Some('=') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::GreaterEqual))
            },
            _ => Some(TokenKind::Operator(OperatorType::Greater))
        },
        '<' => match iter.peek() {
            Some('<') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::LShift))
            },
            Some('=') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::LessEqual))
            },
            _ => Some(TokenKind::Operator(OperatorType::Less))
        },
        '=' => match iter.peek() {
            Some('=') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::Equal))
            },
            _ => Some(TokenKind::Assignment(None))
        },
        ',' => Some(TokenKind::Operator(OperatorType::Comma)),
        _ => {
            iter.back();
            None
        }
    }?;
    
    Some(
        Token {
            kind,
            line: iter.line,
            start_index,
            end_index: iter.current_iter,
        }
    )
}

fn punctuator_lex(iter: &mut CharIter) -> Option<Token> {
    if !iter.has_next() {
        return None;
    }

    let start_index = iter.current_iter;
    let kind = match iter.next().unwrap() {
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
    }?;
    
    Some(
        Token {
            kind,
            line: iter.line,
            start_index,
            end_index: iter.current_iter,
        }
    )
}