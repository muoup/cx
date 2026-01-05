use cranelift::codegen::ir;
use cx_bytecode_data::types::{BCFloatType, BCIntegerType, BCType, BCTypeKind};

pub(crate) fn get_cranelift_abi_type(val_type: &BCType) -> ir::AbiParam {
    ir::AbiParam::new(get_cranelift_type(val_type))
}

pub(crate) fn get_cranelift_type(val_type: &BCType) -> ir::Type {
    match &val_type.kind {
        BCTypeKind::Bool => ir::types::I8,

        BCTypeKind::Integer(BCIntegerType::I8) => ir::types::I8,
        BCTypeKind::Integer(BCIntegerType::I16) => ir::types::I16,
        BCTypeKind::Integer(BCIntegerType::I32) => ir::types::I32,
        BCTypeKind::Integer(BCIntegerType::I64) => ir::types::I64,
        BCTypeKind::Integer(BCIntegerType::I128) => ir::types::I128,

        // BCTypeKind::Float { bytes: 2 } => ir::types::F16,
        BCTypeKind::Float(BCFloatType::F32) => ir::types::F32,
        BCTypeKind::Float(BCFloatType::F64) => ir::types::F64,
        // BCTypeKind::Float { bytes: 16 } => ir::types::F128,
        // 
        BCTypeKind::Union { .. }
        | BCTypeKind::Struct { .. }
        | BCTypeKind::Pointer { .. }
        | BCTypeKind::Array { .. } => ir::Type::int(64).unwrap(),

        // Because of the way Cranelift codegen works, there is actually no need for
        // handling arrays, as anywhere where the type is used (i.e. in stack allocations)
        // will implicitly use the size which can be derived from the bc type.
        _ => panic!("PANIC: Unsupported type for Cranelift: {val_type:?}"),
    }
}
