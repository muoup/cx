use cranelift::codegen::ir;
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_util::{CXError, CXResult};

pub(crate) fn get_cranelift_abi_type(val_type: &LMIRType) -> CXResult<ir::AbiParam> {
    get_cranelift_type(val_type).map(ir::AbiParam::new)
}

pub(crate) fn get_cranelift_type(val_type: &LMIRType) -> CXResult<ir::Type> {
    Ok(match &val_type.kind {
        LMIRTypeKind::Integer(LMIRIntegerType::I1) => ir::types::I8,
        LMIRTypeKind::Integer(LMIRIntegerType::I8) => ir::types::I8,
        LMIRTypeKind::Integer(LMIRIntegerType::I16) => ir::types::I16,
        LMIRTypeKind::Integer(LMIRIntegerType::I32) => ir::types::I32,
        LMIRTypeKind::Integer(LMIRIntegerType::I64) => ir::types::I64,
        LMIRTypeKind::Integer(LMIRIntegerType::I128) => ir::types::I128,

        // LMIRTypeKind::Float { bytes: 2 } => ir::types::F16,
        LMIRTypeKind::Float(LMIRFloatType::F32) => ir::types::F32,
        LMIRTypeKind::Float(LMIRFloatType::F64) => ir::types::F64,
        LMIRTypeKind::Vector { element, count } => {
            let element = get_cranelift_type(element)?;
            element.by(*count as u32).ok_or_else(|| {
                CXError::create_boxed(format!(
                    "Unsupported vector type for codegen: {element} x {count}"
                ))
            })?
        }
        LMIRTypeKind::ABIAggregate { .. } => {
            return CXError::create_result(format!(
                "ABI aggregate must be expanded before scalar codegen: {val_type:?}"
            ));
        }
        // LMIRTypeKind::Float { bytes: 16 } => ir::types::F128,
        //
        LMIRTypeKind::Union { .. }
        | LMIRTypeKind::Struct { .. }
        | LMIRTypeKind::Pointer { .. }
        | LMIRTypeKind::Array { .. } => ir::Type::int(64).unwrap(),

        // FIXME: This is a bit of a hack, but for opaque types we can just treat them as an integer of the appropriate size.
        LMIRTypeKind::Opaque { .. } => ir::Type::int(64).unwrap(),

        // Because of the way Cranelift codegen works, there is actually no need for
        // handling arrays, as anywhere where the type is used (i.e. in stack allocations)
        // will implicitly use the size which can be derived from the bc type.
        _ => return CXError::create_result(format!("Unsupported type for codegen: {val_type:?}")),
    })
}
