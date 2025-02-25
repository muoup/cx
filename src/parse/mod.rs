use crate::lex::token::Token;
use crate::parse::parser::{ParserData, TokenIter, VarTable};
use crate::parse::verify::VerifiedAST;

// pub mod verify;
pub mod ast;
pub mod verify;

mod parser;
mod expression;
mod global_scope;
mod macros;
mod contextless_expression;



pub fn parse_ast(toks: &[Token]) -> Option<VerifiedAST> {
    let mut parser_data = ParserData {
        toks: TokenIter {
            slice: toks,
            index: 0
        },
        vars: VarTable::new()
    };

    let ast = parser::parse_ast(&mut parser_data)?;

    println!("{:#?}", ast);

    let verified_ast = verify::verify_ast(ast)?;

    println!("{:#?}", verified_ast);

    Some(verified_ast)
}