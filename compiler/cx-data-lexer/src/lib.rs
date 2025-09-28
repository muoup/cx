use crate::token::Token;
use std::path::PathBuf;

pub mod token;
pub mod format;

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
}