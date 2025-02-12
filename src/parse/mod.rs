use crate::lex::token::Token;
use crate::parse::parser::TokenIter;

// pub mod verify;
pub mod ast;

mod parser;
mod expression;
mod global_scope;
mod macros;

pub fn parse_ast(toks: &[Token]) -> Option<()> {
    let mut ast = parser::parse_ast(&mut TokenIter {
        slice: toks,
        index: 0
    })?;

    println!("{:#?}", ast);

    // let verified_ast = verify::verify_ast(ast).expect("Failed to verify AST");

    // println!("{:#?}", verified_ast);

    Some(())
}