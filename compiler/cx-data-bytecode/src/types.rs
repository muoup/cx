use std::env::args;
use crate::builder::{BytecodeFunctionPrototype, BytecodeParameter};
use cx_data_ast::parse::ast::TypeMap;
use cx_data_ast::parse::value_type::{CXTypeUnion, CXValType};

pub fn type_to_prototype(
    type_map: &TypeMap,
    cx_type: &CXValType
) -> BytecodeFunctionPrototype {
    let intrin = cx_type.intrinsic_type(type_map);

    let Some(CXTypeUnion::Function { return_type, args })
        = cx_type.intrinsic_type(type_map) else {
        panic!("Expected function type, got: {:?}", cx_type);
    };

    let args = args
        .iter()
        .map(|arg|
            Some(
                BytecodeParameter {
                    name: None,
                    type_: arg.intrinsic_type(type_map)?.clone().to_val_type()
                }
            )
        )
        .collect::<Option<Vec<_>>>().unwrap();

    BytecodeFunctionPrototype {
        name: String::new(),
        return_type: *return_type.clone(),
        args
    }
}