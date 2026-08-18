use cx_mir::{MIRFloatType, MIRIntType};
use cx_thir::thir::r#type::{THIRFloatType, THIRIntType};

pub(crate) fn lower_int_type(ty: THIRIntType) -> MIRIntType {
    match ty {
        THIRIntType::I1 => MIRIntType::I1,
        THIRIntType::I8 => MIRIntType::I8,
        THIRIntType::I16 => MIRIntType::I16,
        THIRIntType::I32 => MIRIntType::I32,
        THIRIntType::I64 => MIRIntType::I64,
        THIRIntType::I128 => MIRIntType::I128,
    }
}

pub(crate) fn lower_float_type(ty: THIRFloatType) -> MIRFloatType {
    match ty {
        THIRFloatType::F32 => MIRFloatType::F32,
        THIRFloatType::F64 => MIRFloatType::F64,
    }
}
