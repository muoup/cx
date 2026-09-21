use cx_util::dense_id;

use crate::unit::{MIRBasicBlockID, MIRGlobalID};

pub use crate::constant::{MIRConstant, MIRConstantID};

dense_id!(MIRPlaceID);
dense_id!(MIRRegisterID);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRValue {
    Register(MIRRegisterID),
    PlaceRef(MIRPlaceID),
    Global(MIRGlobalID),
    Constant(MIRConstantID),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRBindable {
    Register(MIRRegisterID),
    Place(MIRPlaceID),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRTarget {
    Place(MIRPlaceID),
    Global(MIRGlobalID),
    Register(MIRRegisterID),
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
