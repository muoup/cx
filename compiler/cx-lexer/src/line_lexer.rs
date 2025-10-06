use cx_lexer_data::punctuator;
use cx_lexer_data::token::{OperatorType, PunctuatorType, Token, TokenKind};
use cx_util::char_iter::CharIter;

pub(crate) struct LineLexer<'a> {
    last_consume: usize,
    iter: &'a mut CharIter<'a>,

    pub tokens: Vec<Token>,
}

pub(crate) fn lex_line<'a>(iter: &'a mut CharIter<'a>) -> Option<Vec<Token>> {
    let mut line_lexer = LineLexer::new(iter);
    line_lexer.generate_tokens();
    Some(line_lexer.tokens)
}

impl<'a> LineLexer<'a> {
    fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub(crate) fn generate_tokens(&mut self) {
        while self.iter.has_next() && self.iter.peek() != Some('\n') {
            if self.last_consume == self.iter.current_iter {
                if let Some(token) = self.pre_ident_lex() {
                    self.tokens.push(token);
                    self.last_consume = self.iter.current_iter;
                }
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
    }

    fn new(iter: &'a mut CharIter<'a>) -> LineLexer<'a> {
        let last_consume = iter.current_iter;
        LineLexer {
            iter,
            last_consume,
            tokens: Vec::new(),
        }
    }

    fn consume(&mut self, up_to: usize) {
        if up_to == self.last_consume {
            return;
        }

        let str = self.iter.source[self.last_consume..up_to].to_string();
        let str_start = self.last_consume;

        self.last_consume = self.iter.current_iter;

        if str.chars().any(|c| !c.is_whitespace()) {
            let kind = TokenKind::from_str(str);

            self.add_token(Token {
                kind,
                line: self.iter.line,
                start_index: str_start,
                end_index: up_to,
            })
        }
    }

    fn pre_ident_lex(&mut self) -> Option<Token> {
        match self.iter.peek()? {
            '0'..='9' => number_lex(self.iter),
            '"' => string_lex(self.iter),
            '\'' => char_lex(self.iter),
            _ => None,
        }
    }
}

fn number_lex(iter: &mut CharIter) -> Option<Token> {
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
    let num = &iter.source[start_index..iter.current_iter];
    let kind = if dot {
        TokenKind::FloatLiteral(num.parse().unwrap())
    } else {
        TokenKind::IntLiteral(
            num.parse()
                .unwrap_or_else(|_| panic!("Invalid number: {num}\n")),
        )
    };

    Some(Token {
        kind,
        line: iter.line,
        start_index,
        end_index: iter.current_iter,
    })
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
    let string = iter.source[start_iter..iter.current_iter - 1]
        .replace("\\n", "\n")
        .replace("\\t", "\t")
        .replace("\\r", "\r")
        .replace("\\\"", "\"");

    Some(Token {
        kind: TokenKind::StringLiteral(string),

        line: iter.line,
        start_index: start_iter,
        end_index: iter.current_iter - 1,
    })
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
        }
        'n' => {
            assert_eq!(c, '\\');
            assert_eq!(iter.next(), Some('\''));
            TokenKind::IntLiteral('\n' as i64)
        }
        't' => {
            assert_eq!(c, '\\');
            assert_eq!(iter.next(), Some('\''));
            TokenKind::IntLiteral('\t' as i64)
        }
        'r' => {
            assert_eq!(c, '\\');
            assert_eq!(iter.next(), Some('\''));
            TokenKind::IntLiteral('\r' as i64)
        }
        _ => panic!("Invalid character literal: '{c}'"),
    };

    Some(Token {
        kind,
        line: iter.line,
        start_index,
        end_index: iter.current_iter,
    })
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
            Some('>') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::DoubleGT))
            }
            Some('=') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::GreaterEqual))
            }
            _ => Some(TokenKind::Operator(OperatorType::Greater)),
        },
        '<' => match iter.peek() {
            Some('<') => {
                iter.next();
                Some(TokenKind::Operator(OperatorType::DoubleLT))
            }
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
    }?;

    Some(Token {
        kind,
        line: iter.line,
        start_index,
        end_index: iter.current_iter,
    })
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

    Some(Token {
        kind,
        line: iter.line,
        start_index,
        end_index: iter.current_iter,
    })
}
