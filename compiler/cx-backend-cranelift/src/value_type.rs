use cranelift::codegen::ir;
use cx_mir_data::types::{MIRType, MIRTypeKind};

pub(crate) fn get_cranelift_abi_type(val_type: &MIRType) -> ir::AbiParam {
    ir::AbiParam::new(get_cranelift_type(val_type))
}

pub(crate) fn get_cranelift_type(val_type: &MIRType) -> ir::Type {
    match &val_type.kind {
        MIRTypeKind::Signed { bytes } | MIRTypeKind::Unsigned { bytes } if *bytes == 0 => {
            ir::Type::int(8).expect("PANIC: Invalid integer size: 0 bytes")
        }

        MIRTypeKind::Bool => ir::types::I8,

        MIRTypeKind::Signed { bytes } | MIRTypeKind::Unsigned { bytes } => {
            ir::Type::int(*bytes as u16 * 8)
                .unwrap_or_else(|| panic!("PANIC: Invalid integer size: {} bytes", *bytes))
        }

        MIRTypeKind::Float { bytes: 2 } => ir::types::F16,
        MIRTypeKind::Float { bytes: 4 } => ir::types::F32,
        MIRTypeKind::Float { bytes: 8 } => ir::types::F64,
        MIRTypeKind::Float { bytes: 16 } => ir::types::F128,

        MIRTypeKind::Union { .. } | MIRTypeKind::Struct { .. } | MIRTypeKind::Pointer { .. } => {
            ir::Type::int(64).unwrap()
        }

        // Because of the way Cranelift codegen works, there is actually no need for
        // handling arrays, as anywhere where the type is used (i.e. in stack allocations)
        // will implicitly use the size which can be derived from the bc type.
        _ => panic!("PANIC: Unsupported type for Cranelift: {val_type:?}"),
    }
}
