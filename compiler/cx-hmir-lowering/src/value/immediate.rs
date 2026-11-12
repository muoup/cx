use cx_hmir::ty::HMIRTypeID;
use cx_util::unsafe_float::FloatWrapper;

use crate::value::{
    HMIRValue,
    address::{Addressable, HMIRAddress},
    staged::QuoteRef,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRImmediate {
    Unit,

    Address(HMIRAddress<Addressable>),

    Int(i128),
    Float(FloatWrapper),
    Aggregate {
        fields: Vec<HMIRImmediate>,
    },
    /// A union holding one of its members, tagged or not; the layout of its type is constructed where it is stored
    Union {
        variant: usize,
        payload: Box<HMIRImmediate>,
    },

    Quote(QuoteRef),

    /// The function constructing a variant of a tagged union. It is generated only where it is lowered as a
    /// runtime operand; a comptime call constructs the variant in place
    Constructor {
        ty: HMIRTypeID,
        variant: usize,
    },
    Curried {
        callee: Box<HMIRValue>,
        args: Vec<Option<HMIRValue>>,
    },

    Type(HMIRTypeID),

    /// A value of a comptime-only type whose contents are unknown, standing in where nothing is evaluated
    Opaque,
}
