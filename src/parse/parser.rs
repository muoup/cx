use std::iter::Peekable;
use std::slice;

use crate::lex::token::PunctuatorType::{CloseBrace, CloseParen, Comma, OpenBrace, OpenParen, Semicolon};
use crate::lex::token::{KeywordType, Token};
use crate::parse::ast::{Expression, FunctionDeclaration, Root, AST};
use crate::parse::expression::{parse_expression, parse_expressions};
use crate::parse::val_type::parse_type;

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
}

// pub(crate) type TokenIter<'a> = Peekable<slice::Iter<'a, Token>>;

pub fn parse_ast(toks: &[Token]) -> Option<AST> {
    Some(
        AST {
            root: parse_root(&mut TokenIter {
                slice: toks,
                index: 0
            })?
        }
    )
}

fn parse_root(toks: &mut TokenIter) -> Option<Root> {
    Some(
        Root {
            fn_declarations: parse_fn_declarations(toks)?
        }
    )
}

fn parse_fn_declarations(toks: &mut TokenIter) -> Option<Vec<FunctionDeclaration>> {
    let mut fns = Vec::new();

    while toks.peek() != None {
        fns.push(parse_fn_declaration(toks)?);
    }

    Some(fns)
}

fn parse_fn_declaration(toks: &mut TokenIter) -> Option<FunctionDeclaration> {
    let return_type = parse_type(toks)?;
    let name = match toks.next()? {
        Token::Identifier(name) => name.clone(),
        _ => return panic!("Expected function name")
    };
    assert_eq!(toks.next(), Some(&Token::Punctuator(OpenParen)));

    let arguments = parse_expressions(toks, Token::Punctuator(Comma), Token::Punctuator(CloseParen))?;
    assert!(arguments.iter().all(|arg| matches!(arg, Expression::VariableDeclaration {..})));

    assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));
    let body = parse_body(toks);
    Some(
        FunctionDeclaration {
            return_type,
            arguments,
            name,
            body
        }
    )
}

pub(crate) fn parse_body(toks: &mut TokenIter) -> Vec<Expression> {
    if toks.peek() == Some(&&Token::Punctuator(OpenBrace)) {
        toks.next();
        let body = parse_expressions(toks, Token::Punctuator(Semicolon), Token::Punctuator(CloseBrace)).unwrap();
        assert_eq!(toks.next(), Some(&Token::Punctuator(CloseBrace)));

        body
    } else {
        vec![parse_expression(toks).unwrap()]
    }
}