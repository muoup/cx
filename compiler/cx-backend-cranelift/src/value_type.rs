use cranelift::codegen::ir;
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_log::{error::message::CXStdErrMessage, CXRawResult};

pub(crate) fn get_cranelift_abi_type(val_type: &LMIRType) -> CXRawResult<ir::AbiParam> {
    get_cranelift_type(val_type).map(ir::AbiParam::new)
}

pub(crate) fn get_cranelift_type(val_type: &LMIRType) -> CXRawResult<ir::Type> {
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
            let element = match element {
                LMIRFloatType::F32 => ir::types::F32,
                LMIRFloatType::F64 => ir::types::F64,
            };

            element.by(*count as u32).ok_or_else(|| {
                CXStdErrMessage::error(
                    "CODEGEN ERROR",
                    format!("Unsupported vector type for codegen: {element} x {count}"),
                )
            })?
        }
        // LMIRTypeKind::Float { bytes: 16 } => ir::types::F128,
        //
        LMIRTypeKind::Pointer { bytes, .. } => ir::Type::int(*bytes as u16 * 8).unwrap(),

        // Because of the way Cranelift codegen works, there is actually no need for
        // handling arrays, as anywhere where the type is used (i.e. in stack allocations)
        // will implicitly use the size which can be derived from the bc type.
        LMIRTypeKind::Struct { .. }
        | LMIRTypeKind::Array { .. }
        | LMIRTypeKind::Opaque { .. }
        | LMIRTypeKind::Unit => {
            return CXStdErrMessage::result(
                "CODEGEN ERROR",
                format!("Unsupported type for codegen: {val_type:?}"),
            );
        }
    })
}
