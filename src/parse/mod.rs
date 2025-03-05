use std::{alloc, mem};
use crate::lex::token::Token;
use crate::log_error;
use crate::parse::ast_interface::ASTInterface;
use crate::parse::parser::{ParserData, TokenIter, VarTable, VisibilityMode};
use crate::parse::verify::VerifiedAST;

// pub mod verify;
pub mod ast;
pub mod verify;

mod parser;
mod expression;
mod global_scope;
mod macros;
mod contextless_expression;
mod ast_interface;

pub fn parse_ast_interface(toks: &[Token]) -> Option<ASTInterface> {
    let mut parser_data = ParserData {
        visibility: VisibilityMode::Package,
        toks: TokenIter {
            slice: toks,
            index: 0
        },
    };

    let Some(ast) = parser::parse_ast(&mut parser_data) else {
        log_error!("Failed to parse AST");
    };

    None
}

pub fn parse_ast(toks: &[Token]) -> Option<VerifiedAST> {
    let mut parser_data = ParserData {
        visibility: VisibilityMode::Package,
        toks: TokenIter {
            slice: toks,
            index: 0
        },
    };

    let Some(ast) = parser::parse_ast(&mut parser_data) else {
        log_error!("Failed to parse AST");
    };
    let Some(verified_ast) = verify::verify_ast(ast) else {
        log_error!("Failed to verify AST");
    };

    Some(verified_ast)
}