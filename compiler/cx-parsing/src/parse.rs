use cx_parsing_data::parse::{ast::CXAST, parser::{ParserData, VisibilityMode}};
use cx_lexer_data::TokenIter;

use crate::definitions::global_scope::parse_global_stmt;

pub fn parse_ast(iter: TokenIter, base_ast: CXAST) -> Option<CXAST> {
    let mut data = ParserData {
        ast: base_ast,
        tokens: iter,
        visibility: VisibilityMode::Package,
        expr_commas: vec![true],
    };

    while data.tokens.has_next() {
        if let Some(expr) = parse_global_stmt(&mut data)? {
            data.ast.global_stmts.push(expr);
        }
    }

    Some(data.ast)
}
