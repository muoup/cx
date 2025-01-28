use std::iter::Peekable;
use std::slice;

use crate::lex::token::Punctuator::{CloseBrace, CloseParen, Comma, OpenBrace, OpenParen, Semicolon};
use crate::lex::token::{KeywordType, Token};
use crate::parse::ast::{Expression, FunctionDeclaration, Root, AST};

type TokenIter<'a> = Peekable<slice::Iter<'a, Token>>;

pub fn parse_ast(toks: &[Token]) -> Option<AST> {
    Some(
        AST {
            root: parse_root(&mut toks.iter().peekable())?
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
    fns.push(parse_fn_declaration(toks)?);
    Some(fns)
}

fn parse_fn_declaration(toks: &mut TokenIter) -> Option<FunctionDeclaration> {
    let return_type = match &toks.next()? {
        Token::Keyword(keyword) => *keyword,
        _ => return None
    };
    let name = match toks.next()? {
        Token::Identifier(name) => name.clone(),
        _ => return None
    };
    assert_eq!(toks.next(), Some(&Token::Punctuator(OpenParen)));
    assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));
    assert_eq!(toks.next(), Some(&Token::Punctuator(OpenBrace)));
    let body = parse_expressions(
        toks,
        Token::Punctuator(Semicolon), Token::Punctuator(CloseBrace)
    )?;
    assert_eq!(toks.next(), Some(&Token::Punctuator(CloseBrace)));
    Some(
        FunctionDeclaration {
            return_type,
            name,
            body
        }
    )
}

fn parse_expressions(toks: &mut TokenIter, splitter: Token, terminator: Token) -> Option<Vec<Expression>> {
    let mut exprs = Vec::new();

    while let Some(expression) = parse_expression(toks) {
        exprs.push(expression);

        if toks.peek() == Some(&&splitter) {
            toks.next();
        }

        if toks.peek() == Some(&&terminator) {
            return Some(exprs);
        }
    }

    None
}

fn parse_expression(toks: &mut TokenIter) -> Option<Expression> {
    match toks.next()? {
        Token::Keyword(keyword) => parse_keyword_expression(toks, *keyword),
        Token::Identifier(name) => parse_identifier_expression(toks, name.as_str()),

        Token::StringLiteral(value) => Some(Expression::StringLiteral(value.clone())),
        Token::IntLiteral(value) => Some(Expression::IntLiteral(*value)),
        Token::FloatLiteral(value) => Some(Expression::FloatLiteral(*value)),

        other => panic!("Unexpected expression token: {:?}", other)
    }
}

fn parse_keyword_expression(toks: &mut TokenIter, keyword: KeywordType) -> Option<Expression> {
    match keyword {
        KeywordType::Return =>
            Some (
                if toks.peek() == Some(&&Token::Punctuator(Semicolon)) {
                    Expression::Return(Box::new(Expression::Unit))
                } else {
                    Expression::Return(Box::new(parse_expression(toks)?))
                }
            ),
        _ => None
    }
}

fn parse_identifier_expression(toks: &mut TokenIter, name: &str) -> Option<Expression> {
    match toks.next()? {
        // Function call (identifier followed by open paren)
        Token::Punctuator(OpenParen) => {
            let args = parse_expressions(toks, Token::Punctuator(Comma), Token::Punctuator(CloseParen))?;
            assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));
            Some(
                Expression::FunctionCall(
                    Box::new(Expression::Identifier(name.to_string())),
                    args
                )
            )
        },
        _ => None
    }
}