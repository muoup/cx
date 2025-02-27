use cranelift::codegen::ir;
use crate::parse::ast::{ValueType};
pub(crate) fn get_cranelift_abi_type(val_type: &ValueType) -> ir::AbiParam {
    ir::AbiParam::new(get_cranelift_type(val_type))
}

pub(crate) fn get_cranelift_type(val_type: &ValueType) -> ir::Type {
    match val_type {
        ValueType::Integer { bytes, .. } => {
            match bytes {
                1 => ir::types::I8,
                2 => ir::types::I16,
                4 => ir::types::I32,
                8 => ir::types::I64,
                _ => panic!("Invalid integer size")
            }
        },
        ValueType::Float { bytes } => {
            match bytes {
                4 => ir::types::F32,
                8 => ir::types::F64,
                _ => panic!("Invalid float size")
            }
        },
        ValueType::PointerTo(_) | ValueType::Array { .. } | ValueType::Structured { .. } => ir::types::I64,
        ValueType::Unit => ir::types::INVALID,

        _ => panic!("Unverified type: {:#?}", val_type),
    }
}