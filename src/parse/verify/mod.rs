use std::collections::HashMap;
use std::sync::Arc;
use crate::parse::ast::{ValueType, Root, AST};
use crate::parse::verify::context::{FunctionPrototype, VerifyContext};

pub mod context;
mod global_pass;
mod local_pass;
mod type_verification;

// ValueType vs ValueType ->
//      ValueType    - Represents a type, either intrinsic or user-defined struct/enum types
//      ValueType   - Complex data type, allows pointers and arrays, the actual type of values
pub type ValueTypeRef = Arc<ValueType>;

pub struct VerifiedAST {
    pub root: Root,
    pub functions: HashMap<String, FunctionPrototype>,
    pub named_typedefs: HashMap<String, ValueTypeRef>
}

fn base_type_defs() -> HashMap<String, ValueTypeRef> {
    let mut type_defs = HashMap::new();

    type_defs.insert("i8" .to_owned(), ValueTypeRef::new(ValueType::Integer { bytes: 1, signed: true }));
    type_defs.insert("i16".to_owned(), ValueTypeRef::new(ValueType::Integer { bytes: 2, signed: true }));
    type_defs.insert("i32".to_owned(), ValueTypeRef::new(ValueType::Integer { bytes: 4, signed: true }));
    type_defs.insert("i64".to_owned(), ValueTypeRef::new(ValueType::Integer { bytes: 8, signed: true }));
    type_defs.insert("u8" .to_owned(), ValueTypeRef::new(ValueType::Integer { bytes: 1, signed: false }));
    type_defs.insert("u16".to_owned(), ValueTypeRef::new(ValueType::Integer { bytes: 2, signed: false }));
    type_defs.insert("u32".to_owned(), ValueTypeRef::new(ValueType::Integer { bytes: 4, signed: false }));
    type_defs.insert("u64".to_owned(), ValueTypeRef::new(ValueType::Integer { bytes: 8, signed: false }));
    type_defs.insert("f32".to_owned(), ValueTypeRef::new(ValueType::Float   { bytes: 4 }));
    type_defs.insert("f64".to_owned(), ValueTypeRef::new(ValueType::Float   { bytes: 8 }));

    type_defs.insert("int".to_owned(), type_defs.get("i32").unwrap().clone());
    type_defs.insert("unsigned int".to_owned(), type_defs.get("u32").unwrap().clone());
    type_defs.insert("float".to_owned(), type_defs.get("f32").unwrap().clone());
    type_defs.insert("double".to_owned(), type_defs.get("f64").unwrap().clone());

    type_defs.insert("void".to_owned(), ValueTypeRef::new(ValueType::Unit));

    type_defs
}

pub fn verify_ast(mut ast: AST) -> Option<VerifiedAST> {
    let mut context = VerifyContext {
        variable_table: vec![],
        function_table: HashMap::new(),
        types_table: base_type_defs(),

        current_return_type: None
    };

    global_pass::global_pass(&mut context, &ast.root);
    local_pass::local_pass(&mut context, &mut ast.root)?;

    Some(
        VerifiedAST {
            root: ast.root,
            functions: context.function_table,
            named_typedefs: HashMap::new()
        }
    )
}