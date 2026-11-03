pub mod address;
pub mod constant;
pub mod staged;

use rustc_hash::FxHashMap;

use cx_hmir::{HMIRDefID, body::HMIRLocalID, ty::HMIRTypeID};
use cx_util::dense_id;

use crate::value::constant::HMIRConstant;

pub(crate) type Bindings = FxHashMap<HMIRLocalID, HMIRValue>;

dense_id!(HMIRSlotID, "$");
dense_id!(HMIRRegionID, "mem");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRPlace {
    Local(HMIRSlotID),
    Global(HMIRDefID),
    // REVIEW: HMIRRegionID and HMIRSlotID is a faulty distinction, the former is MIR-bound (bad), and the latter
    // is interpreter memory bound, which is overly restricting. Investigate unifying this into clearer categories. 
    Region(HMIRRegionID),
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
    Indirect(Box<HMIRValue>),
}

impl HMIRValue {
    pub fn new(ty: HMIRTypeID, kind: HMIRValueKind) -> Self {
        Self { ty, kind }
    }

    pub fn constant(ty: HMIRTypeID, constant: HMIRConstant) -> Self {
        Self::new(ty, HMIRValueKind::Constant(constant))
    }

    pub fn curried(ty: HMIRTypeID, callee: Self, args: Vec<Option<Self>>) -> Self {
        let callee = Box::new(callee);
        Self::constant(ty, HMIRConstant::Curried { callee, args })
    }

    pub fn place(ty: HMIRTypeID, place: HMIRPlace) -> Self {
        Self::new(ty, HMIRValueKind::PlaceRef(place))
    }

    pub fn indirect(ty: HMIRTypeID, pointer: HMIRValue) -> Self {
        match &pointer.kind {
            HMIRValueKind::Constant(HMIRConstant::Address(address)) => {
                match address.whole_place() {
                    Some(place) => Self::place(ty, place),
                    None => Self::new(ty, HMIRValueKind::Indirect(Box::new(pointer))),
                }
            }
            _ => Self::new(ty, HMIRValueKind::Indirect(Box::new(pointer))),
        }
    }

    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn kind(&self) -> &HMIRValueKind {
        &self.kind
    }

    pub fn as_type(&self) -> Option<HMIRTypeID> {
        match self.kind {
            HMIRValueKind::Constant(HMIRConstant::Type(ty)) => Some(ty),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i128> {
        match self.kind {
            HMIRValueKind::Constant(HMIRConstant::Int(value)) => Some(value),
            _ => None,
        }
    }

    pub fn with_ty(mut self, ty: HMIRTypeID) -> Self {
        self.ty = ty;
        self
    }
}
