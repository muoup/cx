use cx_data_ast::parse::{ast::CXAST, parser::ParserData};
use cx_data_lexer::TokenIter;

use crate::definitions::global_scope::parse_global_stmt;

pub fn parse_ast(iter: TokenIter, base_ast: CXAST) -> Option<CXAST> {
    let mut data = ParserData {
        ast: base_ast,
        tokens: iter,
        visibility: cx_data_ast::parse::parser::VisibilityMode::Package,
        expr_commas: vec![true],
    };
    
    while data.tokens.has_next() {
        if let Some(expr) = parse_global_stmt(&mut data)? {
            data.ast.global_stmts.push(expr);
        }
    }
    
    Some(data.ast)
}