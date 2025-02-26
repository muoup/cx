use crate::lex::token::PunctuatorType::{CloseBrace, OpenBrace, Semicolon};
use crate::lex::token::Token;
use crate::parse::ast::{Expression, ValueType, AST};
use crate::parse::expression::{parse_rvals, parse_rvalue};
use crate::parse::global_scope::parse_global_stmt;
use crate::util::ScopedMap;

pub(crate) type VarTable = ScopedMap<ValueType>;

#[derive(Debug, Clone)]
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

pub(crate) fn parse_ast(data: &mut ParserData) -> Option<AST> {
    let mut ast = AST {
        statements: Vec::new()
    };

    while data.toks.peek() != None {
        let stmt = parse_global_stmt(data)?;

        // println!("{:?}", stmt);

        ast.statements.push(stmt);
    }

    Some(ast)
}

pub(crate) fn parse_body(data: &mut ParserData) -> Vec<Expression> {
    if data.toks.peek() == Some(&&Token::Punctuator(OpenBrace)) {
        data.toks.next();

        let body = parse_rvals(data, Token::Punctuator(Semicolon), Token::Punctuator(CloseBrace)).unwrap();
        assert_eq!(data.toks.next(), Some(&Token::Punctuator(CloseBrace)));

        body
    } else {
        let body = vec![parse_rvalue(data).unwrap()];

        body
    }
}