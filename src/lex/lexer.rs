use std::io::BufRead;
use crate::lex::token::{OperatorType, PunctuatorType, Token};

pub(crate) struct Lexer<'a> {
    source: &'a str,

    last_consume: usize,
    iter: CharIter<'a>,

    pub tokens: Vec<Token>,
}

struct CharIter<'a> {
    source: &'a str,
    current_iter: usize,
}

impl CharIter<'_> {
    fn next(&mut self) -> Option<char> {
        if let Some(c) = self.source.chars().nth(self.current_iter) {
            self.current_iter += 1;
            Some(c)
        } else {
            None
        }
    }
    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current_iter)
    }

    fn back(&mut self) {
        self.current_iter -= 1;
    }

    fn has_next(&self) -> bool {
        self.source.chars().nth(self.current_iter).is_some()
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
                self.tokens.push(operator);
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
            self.tokens.push(Token::from_str(str));
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
    if dot {
        Some(Token::FloatLiteral(num.parse().unwrap()))
    } else {
        Some(Token::IntLiteral(num.parse().expect(&format!("Invalid number: {}\n", num))))
    }
}

fn string_lex(iter: &mut CharIter) -> Option<Token> {
    assert_eq!(iter.next(), Some('"'));
    let mut end = 0;
    for (i, c) in iter.source.chars().enumerate() {
        if c == '"' {
            break;
        }
        end = i;
    }
    let string = &iter.source[..=end];
    Some(Token::StringLiteral(string.to_string()))
}

fn char_lex(iter: &mut CharIter) -> Option<Token> {
    assert_eq!(iter.next(), Some('\''));
    let c = iter.next()?;

    if c != '\\' {
        Some(Token::IntLiteral(c as i64))
    } else {
        let break_char = iter.next()?;

        match break_char {
            'n' => Some(Token::IntLiteral('\n' as i64)),
            't' => Some(Token::IntLiteral('\t' as i64)),
            'r' => Some(Token::IntLiteral('\r' as i64)),
            _ => panic!("Invalid escape character")
        }
    }
}

fn operator_lex(iter: &mut CharIter) -> Option<Token> {
    fn found_operator(iter: &mut CharIter, operator: OperatorType) -> Option<Token> {
        if Some('=') == iter.peek() {
            iter.next();
            Some(Token::Assignment(Some(operator)))
        } else {
            Some(Token::Operator(operator))
        }
    }

    match iter.next()? {
        '+' => found_operator(iter, OperatorType::Add),
        '-' => found_operator(iter, OperatorType::Subtract),
        '*' => found_operator(iter, OperatorType::Multiply),
        '/' => found_operator(iter, OperatorType::Divide),
        '%' => found_operator(iter, OperatorType::Modulo),

        '>' => match iter.peek() {
            Some('>') => {
                iter.next();
                found_operator(iter, OperatorType::RShift)
            },
            _ => found_operator(iter, OperatorType::GreaterThan)
        },
        '<' => match iter.peek() {
            Some('<') => {
                iter.next();
                found_operator(iter, OperatorType::LShift)
            },
            _ => found_operator(iter, OperatorType::LessThan)
        },
        '=' => match iter.peek() {
            Some('=') => {
                iter.next();
                Some(Token::Operator(OperatorType::Equal))
            },
            _ => Some(Token::Assignment(None))
        }
        _ => {
            iter.back();
            None
        }
    }
}

fn punctuator_lex(iter: &mut CharIter) -> Option<Token> {
    if !iter.has_next() {
        return None;
    }

    match iter.next().unwrap() {
        '(' => Some(Token::Punctuator(PunctuatorType::OpenParen)),
        ')' => Some(Token::Punctuator(PunctuatorType::CloseParen)),
        '[' => Some(Token::Punctuator(PunctuatorType::OpenBracket)),
        ']' => Some(Token::Punctuator(PunctuatorType::CloseBracket)),
        '{' => Some(Token::Punctuator(PunctuatorType::OpenBrace)),
        '}' => Some(Token::Punctuator(PunctuatorType::CloseBrace)),
        ',' => Some(Token::Punctuator(PunctuatorType::Comma)),
        ';' => Some(Token::Punctuator(PunctuatorType::Semicolon)),
        ':' => Some(Token::Punctuator(PunctuatorType::Colon)),
        '.' => Some(Token::Punctuator(PunctuatorType::Period)),
        '?' => Some(Token::Punctuator(PunctuatorType::QuestionMark)),

        _ => {
            iter.back();
            None
        }
    }
}