pub mod constant;
pub mod scalar;

use cx_hmir::{HMIRDefID, ty::HMIRTypeID};
use cx_util::dense_id;

use crate::value::constant::HMIRConstant;

// One instantiation of a local or temporary, allocated by whichever context evaluates the body; slots, and the
// addresses and values built on them, belong to that evaluation and are never written back into input HMIR
dense_id!(HMIRSlotID, "$");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRPlace {
    Local(HMIRSlotID),
    Global(HMIRDefID),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRValue {
    ty: HMIRTypeID,
    kind: HMIRValueKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRValueKind {
    Constant(HMIRConstant),
    PlaceRef(HMIRPlace),
    // The storage that a pointer or reference value addresses
    Indirect(Box<HMIRValue>),
}

impl HMIRValue {
    pub fn new(ty: HMIRTypeID, kind: HMIRValueKind) -> Self {
        Self { ty, kind }
    }

    pub fn constant(ty: HMIRTypeID, constant: HMIRConstant) -> Self {
        Self::new(ty, HMIRValueKind::Constant(constant))
    }

    pub fn place(ty: HMIRTypeID, place: HMIRPlace) -> Self {
        Self::new(ty, HMIRValueKind::PlaceRef(place))
    }

    pub fn indirect(ty: HMIRTypeID, pointer: HMIRValue) -> Self {
        match pointer.kind {
            HMIRValueKind::Constant(HMIRConstant::Address {
                place, ref path, ..
            }) if path.is_empty() => Self::place(ty, place),
            _ => Self::new(ty, HMIRValueKind::Indirect(Box::new(pointer))),
        }
    }

    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn kind(&self) -> &HMIRValueKind {
        &self.kind
    }

    pub fn with_ty(mut self, ty: HMIRTypeID) -> Self {
        self.ty = ty;
        self
    }
}
