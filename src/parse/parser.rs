use crate::lex::token::PunctuatorType::{CloseBrace, CloseParen, Comma, OpenBrace, OpenParen, Semicolon};
use crate::lex::token::Token;
use crate::parse::ast::{Expression, GlobalStatement, Root};
use crate::parse::ast::MemoryExpression::VariableDeclaration;
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

    pub(crate) fn has_next(&self) -> bool {
        self.slice.get(self.index).is_some()
    }
}

pub(crate) fn parse_root(toks: &mut TokenIter) -> Option<Root> {
    Some(
        Root {
            global_stmts: parse_global_stmts(toks)?
        }
    )
}

fn parse_global_stmts(toks: &mut TokenIter) -> Option<Vec<GlobalStatement>> {
    let mut fns = Vec::new();

    while toks.peek() != None {
        fns.push(parse_fn_declaration(toks)?);
    }

    Some(fns)
}

fn parse_fn_declaration(toks: &mut TokenIter) -> Option<GlobalStatement> {
    let return_type = parse_type(toks)?;
    let name = match toks.next()? {
        Token::Identifier(name) => name.clone(),
        _ => return panic!("Expected function name")
    };
    assert_eq!(toks.next(), Some(&Token::Punctuator(OpenParen)));

    let arguments = parse_expressions(toks, Token::Punctuator(Comma), Token::Punctuator(CloseParen))?;
    assert!(arguments.iter().all(|arg| matches!(arg, Expression::Memory(VariableDeclaration { .. }))));

    assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));

    let body = match toks.peek()? {
        Token::Punctuator(Semicolon) => { toks.next(); None },
        _ => Some(parse_body(toks))
    };

    Some(
        GlobalStatement::Function {
            name,
            arguments: arguments.into_iter().map(|arg| {
                if let Expression::Memory(VariableDeclaration { name, type_ }) = arg {
                    (name, type_)
                } else {
                    panic!("Expected variable declaration")
                }
            }).collect(),
            return_type,
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