use crate::lex::token::Token;
use crate::parse::value_type::CXValType;
use crate::util::ScopedMap;

pub(crate) type VarTable = ScopedMap<CXValType>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum VisibilityMode {
    Package,
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub(crate) struct ParserData<'a> {
    pub(crate) toks: TokenIter<'a>,
    pub(crate) visibility: VisibilityMode,
}

#[derive(Debug, Clone)]
pub(crate) struct TokenIter<'a> {
    pub(crate) slice: &'a [Token],
    pub(crate) index: usize,
}

impl<'a> TokenIter<'_> {
    pub(crate) fn next(&mut self) -> Option<&Token> {
        let next = self.slice.get(self.index);
        self.index += 1;
        next
    }

    pub(crate) fn peek(&self) -> Option<&Token> {
        self.slice.get(self.index)
    }

    pub(crate) fn back(&mut self) {
        self.index -= 1;
    }

    pub(crate) fn has_next(&self) -> bool {
        self.slice.get(self.index).is_some()
    }
}
