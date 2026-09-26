use cx_util::dense_id;

use crate::{
    MIRStagedID, ty::MIRTypeID, unit::{MIRBasicBlockID, MIRGlobalID},
};

pub use crate::constant::MIRConstant;

dense_id!(MIRPlaceID, "%p");
dense_id!(MIRRegisterID, "%r");
dense_id!(MIRComptimeRegisterID, "%cr");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MIRGlobalRef {
    pub global: MIRGlobalID,
    pub offset: i64,
    pub ty: MIRTypeID,
}

// Places and global references are read as their stored value unless a reference or memory-resident
// value is expected, in which case they denote the address; reference-typed registers are read through
// wherever a non-reference value is expected
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRValue {
    Register(MIRRegisterID),
    PlaceRef(MIRPlaceID),
    Constant(MIRConstant),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRBindable {
    Register(MIRRegisterID),
    Place(MIRPlaceID),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MIRLivenessState {
    Available,
    Moved,
    Uninitialized,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRTarget {
    Place(MIRPlaceID),
    Global(MIRGlobalRef),
    Register(MIRRegisterID),
    Indirect(MIRRegisterID),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRComptimeParameter {
    Runtime(MIRPlaceID),
    Comptime(MIRComptimeRegisterID),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRComptimeOperand {
    Runtime(MIRValue),
    Comptime(MIRComptimeRegisterID),
    Known(MIRComptimeValue),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRComptimeValue {
    Constant(MIRConstant),
    Staged(MIRStagedID),
    Caller(MIRValue)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRComptimeOutput {
    Runtime(MIRRegisterID),
    Comptime(MIRComptimeRegisterID),
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
