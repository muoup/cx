use crate::lex::token::Token;
use crate::parse::value_type::{CXType};
use cx_util::scoped_map::{ScopedMap, ScopedSet};
use std::collections::{HashMap, HashSet};
use serde::{Deserialize, Serialize};
use crate::parse::ast::CXAST;
use crate::parse::maps::CXTypeMap;

pub type VarTable = ScopedMap<CXType>;

#[derive(Debug, Clone, PartialEq, Copy, Deserialize, Serialize)]
pub enum VisibilityMode {
    Package,
    Public,
    Private,
}

#[derive(Debug)]
pub struct ParserData<'a> {
    pub tokens: TokenIter<'a>,
    pub visibility: VisibilityMode,
    pub expr_commas: Vec<bool>,
    
    pub ast: CXAST,
}

#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
    pub slice: &'a [Token],
    pub index: usize,
}

impl<'a> TokenIter<'a> {
    pub fn new(slice: &'a [Token]) -> Self {
        TokenIter {
            slice,
            index: 0,
        }
    }
}

impl<'a> ParserData<'a> {
    pub fn back(&mut self) -> &mut Self {
        self.tokens.back();
        self
    }

    pub fn skip(&mut self) -> &mut Self {
        self.tokens.next();
        self
    }

    pub fn reset(&mut self) {
        self.tokens.index = 0;
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
