use cx_data_ast::parse::ast::{CXAST};
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use cx_data_ast::parse::parser::{ParserData, TokenIter};
use global_scope::parse_global_stmt;

pub mod expression;
pub mod global_scope;
pub mod typing;
pub mod operators;
mod parsing_tools;
mod structured_initialization;
mod template;

#[derive(Debug)]
pub struct CXPreASTInfo {
    pub type_map: CXTypeMap,
    pub public_types: Vec<String>,
    pub imports: Vec<String>,
    pub templated_identifiers: Vec<String>
}

pub fn parse_function_prototypes(mut data: ParserData) -> Option<CXFunctionMap> {
    None
}

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

