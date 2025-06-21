use std::collections::{HashMap, HashSet};
pub use cx_compiler_modules::{serialize_function_data, serialize_module_data, serialize_type_data};
use cx_data_ast::parse::ast::{CXGlobalStmt, CXTypeMap, CXAST};
use cx_data_ast::parse::parser::{ParserData, VisibilityMode};
use cx_util::point_log_error;
use global_scope::parse_global_stmt;
use crate::parse::intrinsic_types::add_intrinsic_types;
use crate::parse::typing::parse_types;

pub mod expression;
pub mod global_scope;
pub mod typing;
pub mod operators;
mod parsing_tools;
pub mod intrinsic_types;

pub fn parse_types_and_deps(mut data: ParserData, internal_dir: &str) -> Option<(CXTypeMap, Vec<String>, Vec<String>)> {
    let (mut type_map, public_types, imports) = parse_types(&mut data)?;

    add_intrinsic_types(&mut type_map);

    Some((type_map, public_types, imports))
}

pub fn parse_ast(mut data: ParserData, internal_dir: &str, type_map: CXTypeMap, imports: Vec<String>) -> Option<CXAST> {
    let mut cx_ast = CXAST {
        tokens: data.toks.slice.to_vec(),
        
        file_path: data.file_path.clone(),
        internal_path: internal_dir.to_string(),
        
        imports,
        global_stmts: Vec::new(),

        public_functions: Vec::new(),
        
        type_map,
        function_map: HashMap::new(),
    };

    data.reset();

    while data.toks.has_next() {
        let Some(_) = parse_global_stmt(&mut data, &mut cx_ast) else {
            point_log_error!(data, "PARSER ERROR: Failed to parse global statement");
        };
    }
    
    Some(cx_ast)
}

