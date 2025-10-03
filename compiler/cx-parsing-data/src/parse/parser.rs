use crate::parse::ast::CXAST;
use cx_lexer_data::TokenIter;
use speedy::{Readable, Writable};

#[derive(Debug, Default, Clone, PartialOrd, PartialEq, Eq, Copy, Readable, Writable)]
pub enum VisibilityMode {
    #[default]
    Private,
    Package,
    Public,
}

#[derive(Debug)]
pub struct ParserData<'a> {
    pub tokens: TokenIter<'a>,
    pub visibility: VisibilityMode,
    pub expr_commas: Vec<bool>,

    pub ast: CXAST,
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
        *self
            .expr_commas
            .last()
            .expect("CRITICAL: No comma mode to get!")
    }
}
