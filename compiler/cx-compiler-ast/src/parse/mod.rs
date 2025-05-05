use std::collections::HashMap;
use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::parser::ParserData;
use global_scope::parse_global_stmt;

pub mod expression;
pub mod global_scope;
pub mod typing;
pub mod operators;
mod lvalues;

pub fn parse_ast(mut data: ParserData) -> Option<CXAST> {
    let mut cx_ast = CXAST {
        imports: Vec::new(),
        global_stmts: Vec::new(),

        type_map: HashMap::new(),
        function_map: HashMap::new()
    };

    while data.toks.has_next() {
        parse_global_stmt(&mut data, &mut cx_ast);
    }

    Some(cx_ast)
}

