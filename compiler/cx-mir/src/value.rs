use cx_util::dense_id;

use crate::unit::{MIRBasicBlockID, MIRGlobalID};

dense_id!(MIRPlaceID);
dense_id!(MIRTemporaryID);
dense_id!(MIRRegisterID);

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
    Global {
        global: MIRGlobalID,
        offset: i64,
        ty: MIRTypeID,
    },
    Nullptr {
        ty: MIRTypeID,
    },
    Function(MIRFunctionID),
    Undefined,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRValue {
    Register(MIRRegisterID),
    Place(MIRPlaceID),
    Global(MIRGlobalID),
    Constant(MIRConstant),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRTarget {
    Place(MIRPlaceID),
    Global(MIRGlobalID),
    Indirect(MIRRegisterID),
}

#[derive(Debug, Clone)]
pub struct MIRBlockTarget {
    pub block: MIRBasicBlockID,
    pub args: Vec<MIRValue>,
}

impl MIRBlockTarget {
    pub fn new(block: MIRBasicBlockID) -> Self {
        Self {
            block,
            args: Vec::new(),
        }
    }

    pub fn with_args(block: MIRBasicBlockID, args: Vec<MIRValue>) -> Self {
        Self { block, args }
    }
}