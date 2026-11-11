pub mod address;
pub mod immediate;
pub mod staged;

use cx_hmir::{HMIRDefID, ty::HMIRTypeID};
use cx_intrinsics::Intrinsic;
use cx_util::dense_id;

use crate::value::{
    address::{Addressable, HMIRAddress},
    immediate::HMIRImmediate,
};

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

fn as_addressable(value: &HMIRValue) -> Option<&Addressable> {
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Address(address)) if address.offset() == 0 => {
            Some(address.base())
        }
        _ => None,
    }
}

pub fn as_function(value: &HMIRValue) -> Option<HMIRDefID> {
    match as_addressable(value)? {
        Addressable::Def(function) => Some(*function),
        _ => None,
    }
}

pub fn as_literal(value: &HMIRValue) -> Option<&str> {
    match as_addressable(value)? {
        Addressable::Literal(string) => Some(string),
        _ => None,
    }
}

pub fn as_place(value: &HMIRValue) -> Option<HMIRPlaceID> {
    match value.kind() {
        HMIRValueKind::Reference(reference) if reference.offset() == 0 => Some(*reference.base()),
        _ => None,
    }
}

pub fn curried(ty: HMIRTypeID, callee: HMIRValue, args: Vec<Option<HMIRValue>>) -> HMIRValue {
    let callee = Box::new(callee);
    HMIRValue::new(ty, HMIRImmediate::Curried { callee, args })
}

// A callee apart from the values a curried call has bound to its parameters
pub fn uncurried(value: HMIRValue) -> (HMIRValue, Vec<Option<HMIRValue>>) {
    let ty = value.ty();
    match value.take_kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Curried { callee, args }) => (*callee, args),
        kind => (HMIRValue::new(ty, kind), Vec::new()),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRValue {
    ty: HMIRTypeID,
    kind: HMIRValueKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRValueKind {
    /// Well-formed representation of a full value in any context, which is the form a comptime evaluation
    /// converges to when it hands its value back to runtime
    Immediate(HMIRImmediate),

    /// An address into storage local to the function evaluating it, whether runtime or comptime; it is never
    /// well-formed outside of that function, so a comptime value holding one cannot converge
    Reference(HMIRAddress<HMIRPlaceID>),

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

impl From<HMIRAddress<HMIRPlaceID>> for HMIRValueKind {
    fn from(value: HMIRAddress<HMIRPlaceID>) -> Self {
        HMIRValueKind::Reference(value)
    }
}
