use crate::lex::token::Token;
use crate::parse::value_type::{CXType};
use cx_util::scoped_map::ScopedMap;
use std::collections::{HashMap, HashSet};

pub type VarTable = ScopedMap<CXType>;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum VisibilityMode {
    Package,
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub struct ParserData<'a> {
    pub file_path: String,
    pub toks: TokenIter<'a>,
    pub visibility: VisibilityMode,
    pub expr_commas: Vec<bool>,

    pub type_symbols: HashSet<String>,
}

#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
    pub slice: &'a [Token],
    pub index: usize,
}

impl<'a> ParserData<'a> {
    pub fn new(file_path: String, toks: &'a [Token]) -> Self {
        ParserData {
            file_path,
            toks: TokenIter { slice: toks, index: 0 },
            visibility: VisibilityMode::Package,
            expr_commas: vec![true],

            type_symbols: HashSet::new(),
        }
    }

    pub fn back(&mut self) -> &mut Self {
        self.toks.back();
        self
    }

    pub fn skip(&mut self) -> &mut Self {
        self.toks.next();
        self
    }

    pub fn reset(&mut self) {
        self.toks.index = 0;
    }

    pub fn change_comma_mode(&mut self, expr_comma: bool) {
        self.expr_commas.push(expr_comma);
    }

    pub fn pop_comma_mode(&mut self) {
        if self.expr_commas.is_empty() {
            panic!("CRITICAL: No comma mode to pop!");
        }

        self.expr_commas.pop();
    }

    pub fn get_comma_mode(&self) -> bool {
        *self.expr_commas.last().expect("CRITICAL: No comma mode to get!")
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
