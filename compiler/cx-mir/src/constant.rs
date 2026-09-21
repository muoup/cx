use cx_util::{dense_id, unsafe_float::FloatWrapper};

use crate::{
    staged::MIRStagedID,
    ty::{MIRFloatType, MIRIntType, MIRTypeID},
    unit::{MIRGlobalID, function::MIRFunctionID},
    value::MIRValue,
};

pub mod pool;

dense_id!(MIRConstantID);

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
        fields: Vec<(usize, MIRConstantID)>,
    },
    String(String),
    Global {
        global: MIRGlobalID,
        offset: i64,
        ty: MIRTypeID,
    },
    Nullptr {
        ty: MIRTypeID,
    },
    Function(MIRFunctionID),
    Staged(MIRStagedID),
    RuntimeValue(MIRValue),
    Undefined,
}
