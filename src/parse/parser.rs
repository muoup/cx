use std::collections::HashMap;
use std::rc::Rc;
use crate::lex::token::PunctuatorType::{CloseBrace, OpenBrace, Semicolon};
use crate::lex::token::Token;
use crate::parse::ast::{Expression, FirstPassGlobals, SecondPassGlobals, AST, UnverifiedExpression, UnverifiedGlobalStatement, ValueExpression};
use crate::parse::expression::{parse_expression, parse_lvals, parse_lvalue, parse_rvals, parse_rvalue};
use crate::parse::global_scope::parse_global_stmt;

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

pub(crate) fn parse_ast(toks: &mut TokenIter) -> Option<AST> {
    let mut ast = AST {
        statements: Vec::new()
    };

    while toks.peek() != None {
        let stmt = parse_global_stmt(toks)?;

        // println!("{:?}", stmt);

        ast.statements.push(stmt);
    }

    Some(ast)
}

pub(crate) fn parse_body(toks: &mut TokenIter) -> Vec<Expression> {
    if toks.peek() == Some(&&Token::Punctuator(OpenBrace)) {
        toks.next();

        let body = parse_rvals(toks, Token::Punctuator(Semicolon), Token::Punctuator(CloseBrace)).unwrap();
        assert_eq!(toks.next(), Some(&Token::Punctuator(CloseBrace)));

        body
    } else {
        vec![parse_rvalue(toks).unwrap()]
    }
}