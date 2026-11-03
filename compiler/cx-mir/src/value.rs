use cx_util::dense_id;

use crate::{
    ty::MIRTypeID,
    unit::{MIRBasicBlockID, MIRGlobalID},
};

pub use crate::constant::MIRConstant;

dense_id!(MIRPlaceID, "%p");
dense_id!(MIRRegisterID, "%r");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MIRGlobalRef {
    pub global: MIRGlobalID,
    pub offset: i64,
    pub ty: MIRTypeID,
}

// A register is its own value; places, globals and the storage behind a reference register are read for their
// contents at the operand's type, which for a memory-resident type is the storage itself
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRValue {
    GlobalRef(MIRGlobalRef),
    PlaceRef { place: MIRPlaceID, ty: MIRTypeID },
    Indirect { value: Box<MIRValue>, ty: MIRTypeID },

    Register(MIRRegisterID),
    Constant(MIRConstant),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRBindable {
    Register(MIRRegisterID),
    Place(MIRPlaceID),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRTarget {
    Place(MIRPlaceID),
    Global(MIRGlobalRef),
    Register(MIRRegisterID),
    Indirect { value: MIRValue, ty: MIRTypeID },
}

#[derive(Debug, Clone)]
pub struct MIRBlockTarget {
    pub block: MIRBasicBlockID,
}

impl MIRBlockTarget {
    pub fn new(block: MIRBasicBlockID) -> Self {
        Self { block }
    }
}
