use std::{alloc, mem};
use crate::lex::token::Token;
use crate::log_error;
use crate::parse::ast::AST;
use crate::parse::ast_interface::emit_interface;
use crate::parse::parser::{ParserData, TokenIter, VarTable, VisibilityMode};
use crate::parse::verify::ProgramBytecode;

// pub mod verify;
pub mod ast;
pub mod verify;
pub mod ast_interface;
pub mod unverified;

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
    verify::verify_ast(ast)
}