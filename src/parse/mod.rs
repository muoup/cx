use crate::lex::token::Token;
use crate::parse::parser::TokenIter;

// pub mod verify;
pub mod ast;

mod verify;
mod parser;
mod expression;
mod global_scope;
mod macros;

pub fn parse_ast(toks: &[Token]) -> Option<()> {
    let ast = parser::parse_ast(&mut TokenIter {
        slice: toks,
        index: 0
    })?;

    let verified_ast = verify::verify_ast(ast);

    println!("{:#?}", verified_ast);

    Some(())
}