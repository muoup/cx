use std::rc::Rc;

use cx_hmir::{HMIRDefID, expr::HMIRExprID, ty::HMIRTypeID};
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::value::HMIRPlace;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRConstant {
    Unit,
    Int {
        value: i128,
        ty: HMIRTypeID,
    },
    Float {
        value: FloatWrapper,
        ty: HMIRTypeID,
    },
    Str(String),
    Null(HMIRTypeID),
    Type(HMIRTypeID),
    // A function def with its leading comptime arguments applied
    Function {
        def: HMIRDefID,
        args: Vec<HMIRConstant>,
    },
    Constructor { ty: HMIRTypeID, variant: CXIdent },
    Quote(QuoteRef),
    Aggregate {
        ty: HMIRTypeID,
        fields: Vec<(usize, Box<HMIRConstant>)>,
    },
    // The address of a place, or of the subobject that the field and element indices of 'path' lead to
    Address {
        place: HMIRPlace,
        path: Vec<usize>,
        ty: HMIRTypeID,
    },
    // The address of a label of a runtime function
    LabelAddress {
        function: HMIRDefID,
        label: CXIdent,
        ty: HMIRTypeID,
    },
}

impl HMIRConstant {
    pub fn holds_local_address(&self) -> bool {
        match self {
            Self::Address {
                place: HMIRPlace::Local(_),
                ..
            } => true,
            Self::Aggregate { fields, .. } => {
                fields.iter().any(|(_, field)| field.holds_local_address())
            }
            Self::Function { args, .. } => args.iter().any(Self::holds_local_address),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Quote {
    def: HMIRDefID,
    body: HMIRExprID,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QuoteRef(Rc<Quote>);
