use crate::lex::token::Token;
use crate::parse::ast::AST;
use crate::parse::parser::{ParserData, TokenIter, VarTable, VisibilityMode};
use crate::parse::pass_verified::ProgramBytecode;

// pub mod verify;
pub mod ast;
pub mod pass_verified;
pub mod ast_interface;
pub mod pass_unverified;

pub(crate) mod parser;
mod expression;
mod global_scope;
mod macros;
mod contextless_expression;

pub fn parse_ast(toks: &[Token]) -> Option<AST> {
    let mut parser_data = ParserData {
        visibility: VisibilityMode::Package,
        toks: TokenIter {
            slice: toks,
            index: 0
        },
    };

    parser::parse_ast(&mut parser_data)
}

pub fn verify_ast(ast: AST) -> Option<ProgramBytecode> {
    pass_verified::verify_ast(ast)
}