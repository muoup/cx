use cranelift::codegen::ir;
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};

pub(crate) fn get_cranelift_abi_type(val_type: &LMIRType) -> ir::AbiParam {
    ir::AbiParam::new(get_cranelift_type(val_type))
}

pub(crate) fn get_cranelift_type(val_type: &LMIRType) -> ir::Type {
    match &val_type.kind {
        LMIRTypeKind::Integer(LMIRIntegerType::I1) => ir::types::I8,
        LMIRTypeKind::Integer(LMIRIntegerType::I8) => ir::types::I8,
        LMIRTypeKind::Integer(LMIRIntegerType::I16) => ir::types::I16,
        LMIRTypeKind::Integer(LMIRIntegerType::I32) => ir::types::I32,
        LMIRTypeKind::Integer(LMIRIntegerType::I64) => ir::types::I64,
        LMIRTypeKind::Integer(LMIRIntegerType::I128) => ir::types::I128,

        // LMIRTypeKind::Float { bytes: 2 } => ir::types::F16,
        LMIRTypeKind::Float(LMIRFloatType::F32) => ir::types::F32,
        LMIRTypeKind::Float(LMIRFloatType::F64) => ir::types::F64,
        // LMIRTypeKind::Float { bytes: 16 } => ir::types::F128,
        // 
        LMIRTypeKind::Union { .. }
        | LMIRTypeKind::Struct { .. }
        | LMIRTypeKind::Pointer { .. }
        | LMIRTypeKind::Array { .. } => ir::Type::int(64).unwrap(),

        // Because of the way Cranelift codegen works, there is actually no need for
        // handling arrays, as anywhere where the type is used (i.e. in stack allocations)
        // will implicitly use the size which can be derived from the bc type.
        _ => panic!("PANIC: Unsupported type for Cranelift: {val_type:?}"),
    }
}
