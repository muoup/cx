use cranelift::codegen::ir;
use cx_bytecode_data::types::{BCType, BCTypeKind};

pub(crate) fn get_cranelift_abi_type(val_type: &BCType) -> ir::AbiParam {
    ir::AbiParam::new(get_cranelift_type(val_type))
}

pub(crate) fn get_cranelift_type(val_type: &BCType) -> ir::Type {
    match &val_type.kind {
        BCTypeKind::Signed { bytes } | BCTypeKind::Integer { bytes } if *bytes == 0 => {
            ir::Type::int(8).expect("PANIC: Invalid integer size: 0 bytes")
        }

        BCTypeKind::Bool => ir::types::I8,

        BCTypeKind::Signed { bytes } | BCTypeKind::Integer { bytes } => {
            ir::Type::int(*bytes as u16 * 8)
                .unwrap_or_else(|| panic!("PANIC: Invalid integer size: {} bytes", *bytes))
        }

        BCTypeKind::Float { bytes: 2 } => ir::types::F16,
        BCTypeKind::Float { bytes: 4 } => ir::types::F32,
        BCTypeKind::Float { bytes: 8 } => ir::types::F64,
        BCTypeKind::Float { bytes: 16 } => ir::types::F128,

        BCTypeKind::Union { .. } | BCTypeKind::Struct { .. } | BCTypeKind::Pointer { .. } | BCTypeKind::Array { .. } => {
            ir::Type::int(64).unwrap()
        }

        // Because of the way Cranelift codegen works, there is actually no need for
        // handling arrays, as anywhere where the type is used (i.e. in stack allocations)
        // will implicitly use the size which can be derived from the bc type.
        _ => panic!("PANIC: Unsupported type for Cranelift: {val_type:?}"),
    }
}
