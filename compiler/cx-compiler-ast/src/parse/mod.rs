use std::collections::HashMap;
pub use cx_compiler_modules::{serialize_function_data, serialize_module_data, serialize_type_data};
use cx_data_ast::parse::ast::{CXAST};
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use cx_data_ast::parse::parser::{ParserData, TokenIter};
use global_scope::parse_global_stmt;

pub mod expression;
pub mod global_scope;
pub mod typing;
pub mod operators;
mod parsing_tools;
pub mod intrinsic_types;
mod structured_initialization;
mod template;

#[derive(Debug)]
pub struct CXPreASTInfo {
    pub type_map: CXTypeMap,
    pub public_types: Vec<String>,
    pub imports: Vec<String>,
    pub templated_identifiers: Vec<String>
}

// pub fn parse_types_and_deps(mut data: ParserData) -> Option<CXPreASTInfo> {
//     let mut info = parse_pre_ast_data(&mut data)?;
//
//     if !data.file_path.contains("/lib/std/") {
//         add_intrinsic_imports(&mut info.imports);
//     }
//
//     add_intrinsic_types(&mut info.type_map);
//
//     Some(info)
// }

pub fn parse_function_prototypes(mut data: ParserData) -> Option<CXFunctionMap> {
    None
}

pub fn parse_ast(mut iter: TokenIter, base_ast: CXAST) -> Option<CXAST> {
    let mut data = ParserData {
        tokens: iter,
        visibility: cx_data_ast::parse::parser::VisibilityMode::Package,
        expr_commas: vec![true],
        ast: base_ast,
    };
    
    while data.tokens.has_next() {
        if let Some(expr) = parse_global_stmt(&mut data)? {
            data.ast.global_stmts.push(expr);
        }
    }
    
    Some(data.ast)
}

