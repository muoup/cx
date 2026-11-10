use cx_hmir::{HMIRDefID, ty::HMIRTypeID};
use cx_mir::MIRFunctionID;
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::value::{HMIRValue, staged::QuoteRef};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRImmediate {
    Unit,

    Address(HMIRAddress),

    Int(i128),
    Float(FloatWrapper),
    Aggregate {
        fields: Vec<HMIRImmediate>,
    },
    TaggedUnion {
        variant: usize,
        payload: Box<HMIRImmediate>,
    },

    Quote(QuoteRef),
    Curried {
        callee: Box<HMIRValue>,
        args: Vec<Option<HMIRValue>>,
    },

    Type(HMIRTypeID),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRAddress {
    base: Addressable,
    offset: i64,
}

impl HMIRAddress {
    pub fn new(base: Addressable, offset: i64) -> Self {
        Self { base, offset }
    }

    pub fn base(&self) -> &Addressable {
        &self.base
    }

    pub fn offset(&self) -> i64 {
        self.offset
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Addressable {
    Def(HMIRDefID),
    Label {
        function: MIRFunctionID,
        label: CXIdent,
    },
    Literal(String),
}