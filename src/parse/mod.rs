use crate::lex::token::Token;
use crate::parse::ast::AST;
use crate::parse::parser::TokenIter;

mod parser;
pub mod ast;

pub mod val_type;
mod expression;
mod verify;

pub fn parse_ast(toks: &[Token]) -> Option<AST> {
    let mut ast = AST {
        root: parser::parse_root(&mut TokenIter {
            slice: toks,
            index: 0
        })?
    };

    verify::verify_ast(&mut ast);

    Some(ast)
}