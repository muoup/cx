use std::collections::HashMap;
pub use cx_compiler_modules::{serialize_function_data, serialize_module_data, serialize_type_data};
use cx_data_ast::parse::ast::{CXGlobalStmt, CXTypeMap, CXAST};
use cx_data_ast::parse::parser::ParserData;
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

pub fn parse_types_and_deps(mut data: ParserData, internal_dir: &str) -> Option<(CXTypeMap, Vec<String>)> {
    let (mut type_map, imports) = parse_types(&mut data)?;
    
    serialize_type_data(internal_dir, type_map.iter())
        .expect("Failed to serialize type data");
    
    add_intrinsic_types(&mut type_map);
    
    Some((type_map, imports))
}

pub fn parse_ast(mut data: ParserData, internal_dir: &str, type_map: CXTypeMap, imports: Vec<String>) -> Option<CXAST> {
    let mut cx_ast = CXAST {
        tokens: data.toks.slice.to_vec(),
        
        file_path: data.file_path.clone(),
        internal_path: internal_dir.to_string(),
        
        imports,
        global_stmts: Vec::new(),

        type_map,
        function_map: HashMap::new()
    };

    data.reset();

    while data.toks.has_next() {
        let Some(_) = parse_global_stmt(&mut data, &mut cx_ast) else {
            point_log_error!(data, "PARSER ERROR: Failed to parse global statement");
        };
    }
    
    for stmt in &mut cx_ast.global_stmts {
        match stmt {
            CXGlobalStmt::FunctionDefinition { prototype, .. } => {
                let name = prototype.name.as_string();
                cx_ast.function_map.insert(name.clone(), prototype.clone());
            },
            CXGlobalStmt::FunctionForward { prototype } => {
                let name = prototype.name.as_string();
                cx_ast.function_map.insert(name, prototype.clone());
            },
            _ => {}
        }
    }
    
    serialize_function_data(cx_ast.internal_path.as_str(), cx_ast.function_map.values())
        .expect("Failed to serialize function data");

    Some(cx_ast)
}

