use crate::lex::token::Token;
use std::io::BufRead;

pub struct Lexer<'a> {
    source: &'a str,

    last_consume: usize,
    current_iter: usize,

    pub tokens: Vec<Token>,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source,

            last_consume: 0,
            current_iter: 0,

            tokens: Vec::new(),
        }
    }

    pub fn generate_tokens(&mut self) {
        while self.current_iter < self.source.len() {
            let pre_iter = self.current_iter;
            let next_char = self.peek_char().unwrap();

            if let Some(op) = Token::try_op(next_char)
                .or_else(|| Token::try_punc(next_char)) {
                self.consume(pre_iter);
                self.tokens.push(op);
                self.current_iter += 1;
                self.last_consume = self.current_iter;
            } else if let Some((tok, end)) = Token::try_number(&self.source[self.current_iter..]) {
                self.consume(pre_iter);
                self.tokens.push(tok);
                self.current_iter += end + 1;
                self.last_consume = self.current_iter;
            } else if self.try_next_char(' ') || self.try_next_whitespace() {
                self.consume(pre_iter);
                self.last_consume = self.current_iter;
            } else if self.try_next_char('"') {
                self.consume(pre_iter);
                self.current_iter = self.source[self.current_iter..]
                    .find('"')
                    .map(|i| i + 1 + self.current_iter)
                    .unwrap();
                self.last_consume = self.current_iter;
                self.tokens.push(Token::StringLiteral(
                    self.source[pre_iter + 1..self.current_iter - 1].to_string()
                ));
            } else {
                self.current_iter += 1;
            }
        }

        self.consume(self.current_iter);
    }
    fn consume(&mut self, up_to: usize) {
        if up_to == self.last_consume {
            return;
        }

        let str = self.source[self.last_consume.. up_to].to_string();
        self.last_consume = self.current_iter;

        self.tokens.push(Token::from_str(str));
    }
    fn next_char(&mut self) -> Option<char> {
        if let Some(c) = self.peek_char() {
            self.current_iter += 1;
            Some(c)
        } else {
            None
        }
    }
    fn peek_char(&self) -> Option<char> {
        self.source.chars().nth(self.current_iter)
    }
    fn try_next_char(&mut self, c: char) -> bool {
        if self.peek_char() == Some(c) {
            self.next_char();
            true
        } else {
            false
        }
    }
    fn try_next_whitespace(&mut self) -> bool {
        if self.peek_char().map(|c| c.is_whitespace()) == Some(true) {
            self.next_char();
            true
        } else {
            false
        }
    }
}