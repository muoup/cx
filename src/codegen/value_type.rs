use cranelift::codegen::ir;
use crate::parse::{parser, value_type};
use crate::parse::pass_molded::TypeMap;
use crate::parse::value_type::CXValType;

pub(crate) fn get_cranelift_abi_type(type_map: &TypeMap, val_type: &CXValType) -> ir::AbiParam {
    ir::AbiParam::new(get_cranelift_type(val_type, type_map))
}

pub(crate) fn get_cranelift_type(val_type: &CXValType, type_map: &TypeMap) -> ir::Type {
    match val_type {
        CXValType::Integer { bytes, .. } => {
            match bytes {
                1 => ir::types::I8,
                2 => ir::types::I16,
                4 => ir::types::I32,
                8 => ir::types::I64,
                _ => panic!("Invalid integer size")
            }
        },
        CXValType::Float { bytes } => {
            match bytes {
                4 => ir::types::F32,
                8 => ir::types::F64,
                _ => panic!("Invalid float size")
            }
        },
        CXValType::PointerTo(_) |
        CXValType::Array { .. } |
        CXValType::Structured { .. } => ir::types::I64,

        CXValType::Unit => panic!("Unit type has no representation"),
        CXValType::Identifier(_type) => {
            let Some(type_) = type_map.get(_type) else {
                panic!("Typechecking allowed invalid type {:#?}", _type);
            };
            get_cranelift_type(type_, type_map)
        },

        _ => panic!("Unverified type: {:#?}", val_type),
    }
}