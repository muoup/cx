use cx_data_ast::parse::ast::{CXGlobalStmt, CXAST};

pub fn parse_fn_mappings(cxast: &mut CXAST) {
    for stmt in &mut cxast.global_stmts {
        match stmt {
            CXGlobalStmt::FunctionDefinition { prototype, .. } => {
                let name = prototype.name.as_string();
                cxast.function_map.insert(name.clone(), prototype.clone());
            },
            CXGlobalStmt::FunctionForward { prototype } => {
                let name = prototype.name.as_string();
                cxast.function_map.insert(name, prototype.clone());
            },
            _ => {}
        }
    }
}