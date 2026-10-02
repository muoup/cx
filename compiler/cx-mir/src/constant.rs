use cx_util::{dense_id, unsafe_float::FloatWrapper};

use crate::{
    MIRGlobalRef,
    ty::{MIRFloatType, MIRIntType, MIRTypeID},
    unit::function::MIRFunctionID,
};

dense_id!(MIRStagedID, "%s");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRConstant {
    Unit,
    Integer {
        ty: MIRIntType,
        value: i128,
    },
    Float {
        value: FloatWrapper,
        ty: MIRFloatType,
    },
    Aggregate {
        ty: MIRTypeID,
        fields: Vec<(usize, MIRConstant)>,
    },
    GlobalRef(MIRGlobalRef),
    String(String),
    Nullptr {
        ty: MIRTypeID,
    },
    Function(MIRFunctionID),
    Undefined,
}
