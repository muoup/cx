use cx_mir::{MIRFloatType, MIRFunctionID, MIRIntType, MIRType};
use cx_thir::thir::expression::THIRExpression;
use cx_util::unsafe_float::FloatWrapper;

pub enum MIRComptimeValue<'staged> {
    Integer { val: i128, _ty: MIRIntType, signed: bool },
    Float { val: FloatWrapper, _ty: MIRFloatType },
    FunctionReference(MIRFunctionID),
    Staged {
        expr: &'staged THIRExpression,
        parameters: Vec<MIRStagedParameter>,
    },
}

pub struct MIRStagedParameter {
    ty: MIRType,
}
