use crate::token::{PunctuatorType, Token, TokenKind};
use std::path::PathBuf;

pub mod format;
pub mod token;

#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
    pub slice: &'a [Token],
    pub index: usize,
    pub file: PathBuf,
}

impl<'a> TokenIter<'a> {
    pub fn new(slice: &'a [Token], file: PathBuf) -> Self {
        TokenIter {
            slice,
            index: 0,
            file,
        }
    }

    pub fn next(&mut self) -> Option<&Token> {
        let next = self.slice.get(self.index)?;
        self.index += 1;
        Some(next)
    }

    pub fn peek(&self) -> Option<&Token> {
        self.slice.get(self.index)
    }

    pub fn back(&mut self) {
        self.index -= 1;
    }

    pub fn prev(&self) -> Option<&Token> {
        if self.index == 0 {
            return None;
        }
        self.slice.get(self.index - 1)
    }

    pub fn reset(&mut self) {
        self.index = 0;
    }

    pub fn has_next(&self) -> bool {
        self.slice.get(self.index).is_some()
    }

    pub fn with_index(&mut self, index: usize) -> Self {
        TokenIter {
            slice: self.slice,
            index,
            file: self.file.clone(),
        }
    }

    pub fn goto_statement_end(&mut self) -> Option<()> {
        let mut bracket_stack = 0;

        while let Some(token) = self.next() {
            match token.kind {
                TokenKind::Punctuator(PunctuatorType::OpenBrace) => bracket_stack += 1,
                TokenKind::Punctuator(PunctuatorType::CloseBrace) => {
                    bracket_stack -= 1;

                    if bracket_stack == 0 {
                        if matches!(self.peek(), Some(t) if t.kind == TokenKind::Punctuator(PunctuatorType::Semicolon))
                        {
                            self.next();
                        }
                        break;
                    }
                }
                TokenKind::Punctuator(PunctuatorType::Semicolon) if bracket_stack == 0 => break,

                _ => (),
            }
        }

        Some(())
    }
}
