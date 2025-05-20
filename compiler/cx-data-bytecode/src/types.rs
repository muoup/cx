use crate::builder::{BytecodeFunctionPrototype, BytecodeParameter};
use cx_data_ast::parse::ast::{CXFunctionPrototype, TypeMap};
use cx_data_ast::parse::value_type::{CXTypeUnion, CXValType};

pub fn type_to_prototype(
    type_map: &TypeMap,
    cx_type: &CXValType
) -> BytecodeFunctionPrototype {
    let Some(CXTypeUnion::Function { prototype })
        = cx_type.intrinsic_type(type_map) else {
        panic!("Expected function type, got: {:?}", cx_type);
    };

    prototype_cx2bc(
        type_map,
        prototype
    )
}

pub fn prototype_cx2bc(
    type_map: &TypeMap,
    prototype: &CXFunctionPrototype
) -> BytecodeFunctionPrototype {
    let args = prototype
        .parameters
        .iter()
        .map(|arg|
            Some(
                BytecodeParameter {
                    name: None,
                    type_: arg.type_.intrinsic_type(type_map)?.clone().to_val_type()
                }
            )
        )
        .collect::<Option<Vec<_>>>().unwrap();

    BytecodeFunctionPrototype {
        name: prototype.name.to_owned(),
        return_type: prototype.return_type.clone(),
        args,
        var_args: prototype.var_args,
    }
}