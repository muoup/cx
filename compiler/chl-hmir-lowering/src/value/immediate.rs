use chl_hmir::ty::HMIRTypeID;
use chl_util::unsafe_float::FloatWrapper;

use crate::value::{
    HMIRApplication, HMIRValue, HMIRValueKind,
    address::{Addressable, HMIRAddress},
    staged::QuoteRef,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRImmediate {
    ty: HMIRTypeID,
    value: HMIRImmValue,
}

impl HMIRImmediate {
    pub fn new(ty: HMIRTypeID, value: HMIRImmValue) -> Self {
        Self { ty, value }
    }

    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn value(&self) -> &HMIRImmValue {
        &self.value
    }
}

impl From<HMIRImmediate> for HMIRValue {
    fn from(immediate: HMIRImmediate) -> Self {
        HMIRValue::new(immediate.ty, immediate.value)
    }
}

impl TryFrom<HMIRValue> for HMIRImmediate {
    type Error = HMIRValue;

    fn try_from(value: HMIRValue) -> Result<Self, Self::Error> {
        let ty = value.ty();
        match value.take_kind() {
            HMIRValueKind::Immediate(immediate) => Ok(Self::new(ty, immediate)),
            kind => Err(HMIRValue::new(ty, kind)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRImmValue {
    Unit,

    Address(HMIRAddress<Addressable>),

    Int(i128),
    Float(FloatWrapper),
    Aggregate {
        fields: Vec<HMIRImmValue>,
    },
    /// A union holding one of its members, tagged or not; the layout of its type is constructed where it is stored
    Union {
        variant: usize,
        payload: Box<HMIRImmValue>,
    },

    Quote(QuoteRef),
    /// The function constructing a variant of a tagged union. It is generated only where it is lowered as a
    /// runtime operand; a comptime call constructs the variant in place
    Constructor {
        ty: HMIRTypeID,
        variant: usize,
    },
    Application(HMIRApplication<HMIRImmediate>),

    Type(HMIRTypeID),

    // REVIEW: Do we need this?
    /// A value of a comptime-only type whose contents are unknown, standing in where nothing is evaluated
    Opaque,
}
