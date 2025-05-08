use std::collections::HashMap;
use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::parser::ParserData;
use global_scope::parse_global_stmt;
use crate::parse::typing::parse_types;

pub mod expression;
pub mod global_scope;
pub mod typing;
pub mod operators;
mod lvalues;
mod parsing_tools;

pub fn parse_ast(mut data: ParserData) -> Option<CXAST> {
    let mut cx_ast = CXAST {
        imports: Vec::new(),
        global_stmts: Vec::new(),

        type_map: parse_types(&mut data)?,
        function_map: HashMap::new()
    };

    data.reset();

    while data.toks.has_next() {
        parse_global_stmt(&mut data, &mut cx_ast);
    }

    Some(cx_ast)
}

