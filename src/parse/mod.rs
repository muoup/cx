use crate::lex::token::Token;
use crate::parse::ast::AST;
use crate::parse::parser::TokenIter;
use crate::parse::verify::VerifiedAST;

pub mod verify;
pub mod ast;

mod parser;
mod expression;
mod type_expr;

pub fn parse_ast(toks: &[Token]) -> Option<VerifiedAST> {
    let mut ast = AST {
        root: parser::parse_root(&mut TokenIter {
            slice: toks,
            index: 0
        })?
    };

    println!("{:?}", ast.root);

    let verified_ast = verify::verify_ast(ast);

    verified_ast
}