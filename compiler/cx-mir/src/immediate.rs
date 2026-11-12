use cx_util::unsafe_float::FloatWrapper;

use crate::{
    MIRGlobalID,
    ty::{MIRFloatType, MIRIntType, MIRTypeID},
    unit::function::MIRFunctionID,
};

// Storage whose address is the same in every context, so that it may be named by an immediate
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRAddressable {
    Global(MIRGlobalID),
    Function(MIRFunctionID),
    Block {
        function: MIRFunctionID,
        label: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRImmediate {
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
        fields: Vec<(usize, MIRImmediate)>,
    },
    Address {
        base: MIRAddressable,
        offset: i64,
    },
    String {
        value: String,
        offset: i64,
    },
    Nullptr {
        ty: MIRTypeID,
    },
    Undefined,
}
