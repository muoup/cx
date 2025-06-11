use std::collections::HashMap;
use cx_compiler_modules::serialize_module_data;
use cx_data_ast::parse::ast::{CXGlobalStmt, CXAST};
use cx_data_ast::parse::parser::ParserData;
use cx_util::point_log_error;
use global_scope::parse_global_stmt;
use crate::parse::typing::parse_types;

pub mod expression;
pub mod global_scope;
pub mod typing;
pub mod operators;
mod parsing_tools;

pub fn parse_ast(mut data: ParserData) -> Option<CXAST> {
    let mut cx_ast = CXAST {
        file_path: data.file_path.clone(),
        imports: Vec::new(),
        global_stmts: Vec::new(),

        type_map: parse_types(&mut data)?,
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
    
    serialize_module_data(&cx_ast)
        .expect("Failed to serialize module data");

    Some(cx_ast)
}

