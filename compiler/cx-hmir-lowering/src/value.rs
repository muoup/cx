pub mod immediate;
pub mod staged;

use cx_hmir::ty::HMIRTypeID;
use cx_util::dense_id;

use crate::value::immediate::HMIRImmediate;

dense_id!(HMIRRegisterID, "$");
dense_id!(HMIRPlaceID, "p");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRValue {
    ty: HMIRTypeID,
    kind: HMIRValueKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRValueKind {
    /// Well-formed representation of a full value, all comptime contexts must return values in this form
    Immediate(HMIRImmediate),

    /// Intermediate computed values of a function
    Register(HMIRRegisterID),
    
    /// Addressable storage local to a function
    /// 
    /// Note that this shape was intentionally separated from HMIRImmediate::Address, while it is in essence addressable,
    /// and offset-addresses are well-defined here, they are not valid for a comptime context to return, and there is no
    /// issue of C backcompatibility as local pointers are not representable in comptime contexts like global initializers 
    Place(HMIRPlaceID),
}

impl HMIRValue {
    pub fn new(ty: HMIRTypeID, kind: impl Into<HMIRValueKind>) -> Self {
        Self { ty, kind: kind.into() }
    }

    pub fn with_ty(mut self, ty: HMIRTypeID) -> Self {
        self.ty = ty;
        self
    }
    
    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn kind(&self) -> &HMIRValueKind {
        &self.kind
    }
}

impl From<HMIRImmediate> for HMIRValueKind {
    fn from(value: HMIRImmediate) -> Self {
        HMIRValueKind::Immediate(value)
    }
}

impl From<HMIRRegisterID> for HMIRValueKind {
    fn from(value: HMIRRegisterID) -> Self {
        HMIRValueKind::Register(value)
    }
}

impl From<HMIRPlaceID> for HMIRValueKind {
    fn from(value: HMIRPlaceID) -> Self {
        HMIRValueKind::Place(value)
    }
}