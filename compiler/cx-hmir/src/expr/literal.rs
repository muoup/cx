use cx_util::unsafe_float::FloatWrapper;

use crate::ty::intrinsic::{HMIRFloatType, HMIRIntType};

#[derive(Debug, Clone)]
pub enum HMIRLiteral {
    Unit,
    Int {
        value: i128,
        ty: HMIRIntType,
    },
    Float {
        value: FloatWrapper,
        ty: HMIRFloatType,
    },
    Str(String),
}
