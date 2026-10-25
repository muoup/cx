use cx_hmir::{HMIRDefID, ty::HMIRTypeID};
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::value::{HMIRPlace, staged::QuoteRef};

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
    // A function with its leading comptime arguments applied
    Function {
        function: HMIRFunctionRef,
        args: Vec<HMIRConstant>,
    },
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRFunctionRef {
    Def(HMIRDefID),
    // The constructor of a variant of a concrete tagged union
    Constructor { ty: HMIRTypeID, variant: usize },
}

impl HMIRConstant {
    // The tag of a tagged union, where a zeroed one holds its first variant
    pub fn tag(&self, ty: HMIRTypeID) -> Option<HMIRConstant> {
        let Self::Aggregate { fields, .. } = self else {
            return None;
        };
        let value = fields.last().map_or(0, |(index, _)| *index) as i128;
        Some(Self::Int { value, ty })
    }

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
