use crate::lex::token::Token;
use crate::parse::ast::AST;
use crate::parse::parser::{ParserData, TokenIter, VisibilityMode};
use crate::parse::pass_bytecode::ProgramBytecode;

// pub mod verify;
pub mod ast;
pub mod ast_interface;

pub mod pass_bytecode;
pub mod pass_unverified;
pub mod pass_molded;
pub mod pass_typecheck;

pub(crate) mod parser;
mod expression;
mod global_scope;
pub mod macros;
mod contextless_expression;
mod format;

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
    pass_bytecode::verify_ast(ast)
}