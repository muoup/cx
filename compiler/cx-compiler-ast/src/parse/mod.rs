use std::collections::HashMap;
pub use cx_compiler_modules::{serialize_function_data, serialize_module_data, serialize_type_data};
use cx_data_ast::parse::ast::{CXAST};
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use cx_data_ast::parse::parser::ParserData;
use global_scope::parse_global_stmt;
use crate::parse::intrinsic_types::{add_intrinsic_imports, add_intrinsic_types};
use crate::parse::typing::parse_pre_ast_data;

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

pub fn parse_types_and_deps(mut data: ParserData) -> Option<CXPreASTInfo> {
    let mut info = parse_pre_ast_data(&mut data)?;

    if !data.file_path.contains("/lib/std/") {
        add_intrinsic_imports(&mut info.imports);
    }

    add_intrinsic_types(&mut info.type_map);

    Some(info)
}

pub fn parse_function_prototypes(mut data: ParserData) -> Option<CXFunctionMap> {
    None
}

pub fn parse_ast(mut data: ParserData, internal_dir: &str, type_map: CXTypeMap, imports: Vec<String>) -> Option<CXAST> {
    let mut cx_ast = CXAST {
        tokens: data.toks.slice.to_vec(),
        
        file_path: data.file_path.clone(),
        internal_path: internal_dir.to_string(),
        imports,
        
        global_stmts: Vec::new(),
        
        type_map,
        function_map: CXFunctionMap::new(),
        
        global_variables: HashMap::new(),
    };

    data.reset();

    while data.toks.has_next() {
        if let Some(expr) = parse_global_stmt(&mut data, &mut cx_ast)? {
            cx_ast.global_stmts.push(expr);
        }
    }
    
    Some(cx_ast)
}

