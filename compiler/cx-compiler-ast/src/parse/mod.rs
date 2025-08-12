use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::maps::CXFunctionMap;
use cx_data_ast::parse::parser::ParserData;
use cx_data_lexer::TokenIter;
use global_scope::parse_global_stmt;

pub mod expression;
pub mod global_scope;
pub mod typing;
pub mod operators;
pub mod precontextualizing;
pub mod template;
mod parsing_tools;
mod structured_initialization;

pub fn parse_function_prototypes(data: ParserData) -> Option<CXFunctionMap> {
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

