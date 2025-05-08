use std::collections::HashSet;
use cx_util::scoped_map::ScopedMap;
use crate::lex::token::{KeywordType, Token};
use crate::parse::value_type::CXValType;

pub type VarTable = ScopedMap<CXValType>;

#[derive(Debug, Clone, PartialEq)]
pub enum VisibilityMode {
    Package,
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub struct ParserData<'a> {
    pub type_symbols: HashSet<String>,
    pub toks: TokenIter<'a>,
    pub visibility: VisibilityMode,
}

#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
    pub slice: &'a [Token],
    pub index: usize,
}

impl<'a> ParserData<'a> {
    pub fn back(&mut self) -> &mut Self {
        self.toks.back();
        self
    }

    pub fn reset(&mut self) {
        self.toks.index = 0;
    }
}

impl<'a> TokenIter<'_> {
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

    pub fn has_next(&self) -> bool {
        self.slice.get(self.index).is_some()
    }
}
