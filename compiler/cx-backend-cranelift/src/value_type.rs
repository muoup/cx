use cranelift::codegen::ir;
use cx_data_ast::parse::ast::TypeMap;
use cx_data_ast::parse::value_type::{CXTypeUnion, CXValType};

pub(crate) fn get_cranelift_abi_type(type_map: &TypeMap, val_type: &CXValType) -> ir::AbiParam {
    ir::AbiParam::new(get_cranelift_type(type_map, val_type))
}

pub(crate) fn get_cranelift_type(type_map: &TypeMap, val_type: &CXValType) -> ir::Type {
    match &val_type.internal_type {
        CXTypeUnion::Integer { bytes, .. } => {
            match bytes {
                1 => ir::types::I8,
                2 => ir::types::I16,
                4 => ir::types::I32,
                8 => ir::types::I64,
                _ => panic!("Invalid integer size")
            }
        },
        CXTypeUnion::Float { bytes } => {
            match bytes {
                4 => ir::types::F32,
                8 => ir::types::F64,
                _ => panic!("Invalid float size")
            }
        },

        CXTypeUnion::Function { .. } |
        CXTypeUnion::PointerTo(_) |
        CXTypeUnion::Array { .. } |
        CXTypeUnion::Structured { .. } => ir::types::I64,

        CXTypeUnion::Unit => panic!("Unit type has no representation"),
        CXTypeUnion::Identifier(_type) => {
            let Some(type_) = type_map.get(_type.as_str()) else {
                panic!("Typechecking allowed invalid type {:#?}", _type);
            };
            get_cranelift_type(type_map, type_)
        },

        _ => panic!("Unverified type: {:#?}", val_type),
    }
}