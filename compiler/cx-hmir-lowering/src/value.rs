pub mod immediate;
pub mod staged;

use cx_hmir::ty::HMIRTypeID;
use cx_intrinsics::Intrinsic;
use cx_util::dense_id;

use crate::value::immediate::{Addressable, HMIRFunctionRef, HMIRImmediate};

dense_id!(HMIRRegisterID, "$");
dense_id!(HMIRPlaceID, "p");

pub type HMIRValIntrinsic = Intrinsic<HMIRValue, HMIRTypeID>;

pub fn as_type(value: &HMIRValue) -> Option<HMIRTypeID> {
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Type(ty)) => Some(*ty),
        _ => None,
    }
}

pub fn as_int(value: &HMIRValue) -> Option<i128> {
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Int(int)) => Some(*int),
        _ => None,
    }
}

pub fn as_function(value: &HMIRValue) -> Option<HMIRFunctionRef> {
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Address(address)) if address.offset() == 0 => {
            match address.base() {
                Addressable::Function(function) => Some(*function),
                _ => None,
            }
        }
        _ => None,
    }
}

pub fn as_literal(value: &HMIRValue) -> Option<&str> {
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Address(address)) if address.offset() == 0 => {
            match address.base() {
                Addressable::Literal(string) => Some(string),
                _ => None,
            }
        }
        _ => None,
    }
}

pub fn as_place(value: &HMIRValue) -> Option<HMIRPlaceID> {
    match value.kind() {
        HMIRValueKind::Place(place) => Some(*place),
        _ => None,
    }
}

pub fn curried(ty: HMIRTypeID, callee: HMIRValue, args: Vec<Option<HMIRValue>>) -> HMIRValue {
    let callee = Box::new(callee);
    HMIRValue::new(ty, HMIRImmediate::Curried { callee, args })
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRValue {
    ty: HMIRTypeID,
    kind: HMIRValueKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRValueKind {
    /// Well-formed representation of a full value, all comptime contexts must return values in this form
    Immediate(HMIRImmediate),

    /// Addressable storage local to a function
    /// 
    /// Note that this shape was intentionally separated from HMIRImmediate::Address, while it is in essence addressable,
    /// and offset-addresses are well-defined here, they are not valid for a comptime context to return, and there is no
    /// issue of C backcompatibility as local pointers are not representable in comptime contexts like global initializers 
    Place(HMIRPlaceID),

    /// Intermediate computed values of a function, non-addressable
    Register(HMIRRegisterID),
}

impl HMIRValue {
    pub fn new(ty: HMIRTypeID, kind: impl Into<HMIRValueKind>) -> Self {
        Self { ty, kind: kind.into() }
    }

    pub fn unit() -> Self {
        Self::new(HMIRTypeID::new(0), HMIRImmediate::Unit)
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

    pub fn take_kind(self) -> HMIRValueKind {
        self.kind
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