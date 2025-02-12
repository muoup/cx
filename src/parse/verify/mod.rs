use std::collections::HashMap;
use std::sync::Arc;
use crate::parse::ast::{ValueType, UnverifiedAST, GlobalStatement};
use crate::parse::verify::context::{FunctionPrototype, VerifyContext};

pub mod context;
mod global_pass;
mod local_pass;
mod type_verification;

#[derive(Debug)]
pub struct VerifiedAST {
    pub global_statements: Vec<GlobalStatement>,
    pub functions: HashMap<String, FunctionPrototype>,
    pub named_typedefs: HashMap<String, ValueType>
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

    type_defs.insert("void".to_owned(), ValueTypeRef::new(ValueType::Unit));

    type_defs
}

pub fn verify_ast(mut ast: UnverifiedAST) -> Option<VerifiedAST> {
    let mut context = VerifyContext {
        variable_table: vec![],
        function_table: HashMap::new(),
        types_table: base_type_defs(),

        current_return_type: None
    };

    global_pass::global_pass(&mut context, &ast);
    local_pass::local_pass(&mut context, &mut ast)?;

    Some(
        VerifiedAST {
            global_statements: vec![],
            functions: context.function_table,
            named_typedefs: HashMap::new()
        }
    )
}