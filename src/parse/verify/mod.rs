use crate::parse::ast::{GlobalStatement, UnverifiedAST, ValueType};
use crate::parse::verify::context::VerifyContext;
use std::collections::HashMap;

pub mod context;
mod global_pass;
mod local_pass;
mod typing;

#[derive(Debug)]
pub struct VerifiedAST {
    pub global_statements: Vec<GlobalStatement>,
}

fn base_type_defs() -> HashMap<String, ValueType> {
    let mut type_defs = HashMap::new();

    type_defs.insert("i8" .to_owned(), ValueType::Integer { bytes: 1, signed: true });
    type_defs.insert("i16".to_owned(), ValueType::Integer { bytes: 2, signed: true });
    type_defs.insert("i32".to_owned(), ValueType::Integer { bytes: 4, signed: true });
    type_defs.insert("i64".to_owned(), ValueType::Integer { bytes: 8, signed: true });
    type_defs.insert("u8" .to_owned(), ValueType::Integer { bytes: 1, signed: false });
    type_defs.insert("u16".to_owned(), ValueType::Integer { bytes: 2, signed: false });
    type_defs.insert("u32".to_owned(), ValueType::Integer { bytes: 4, signed: false });
    type_defs.insert("u64".to_owned(), ValueType::Integer { bytes: 8, signed: false });
    type_defs.insert("f32".to_owned(), ValueType::Float   { bytes: 4 });
    type_defs.insert("f64".to_owned(), ValueType::Float   { bytes: 8 });

    type_defs.insert("int".to_owned(), type_defs.get("i32").unwrap().clone());
    type_defs.insert("unsigned int".to_owned(), type_defs.get("u32").unwrap().clone());
    type_defs.insert("float".to_owned(), type_defs.get("f32").unwrap().clone());
    type_defs.insert("double".to_owned(), type_defs.get("f64").unwrap().clone());
    type_defs.insert("char".to_owned(), type_defs.get("i8").unwrap().clone());
    type_defs.insert("unsigned char".to_owned(), type_defs.get("u8").unwrap().clone());

    type_defs.insert("void".to_owned(), ValueType::Unit);

    type_defs
}

pub fn verify_ast(ast: UnverifiedAST) -> Option<VerifiedAST> {
    let mut context = VerifyContext {
        variable_table: vec![],
        function_table: HashMap::new(),
        types_table: base_type_defs(),

        current_return_type: None
    };

    let Some(mut stmts) = global_pass::global_pass(&mut context, ast.statements) else {
        println!("Failed to verify global pass");
        return None;
    };
    let Some(_) = local_pass::local_pass(&mut context, &mut stmts) else {
        println!("Failed to verify local pass");
        // println!("Tree: {:#?}", stmts);

        return None;
    };

    Some(
        VerifiedAST {
            global_statements: stmts
        }
    )
}