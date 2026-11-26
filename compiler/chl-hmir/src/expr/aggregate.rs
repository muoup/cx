use chl_util::{identifier::CHLIdent, unsafe_float::FloatWrapper};

use crate::{body::HMIRLocalID, expr::HMIRExprID};

#[derive(Debug, Clone)]
pub enum HMIRAggregateOp {
    Member {
        base: HMIRExprID,
        name: CHLIdent,
    },
    Index {
        base: HMIRExprID,
        index: HMIRExprID,
    },
    Initialize {
        ty: HMIRExprID,
        fields: Vec<(Option<CHLIdent>, HMIRExprID)>,
    },
    Is {
        value: HMIRExprID,
        pattern: HMIRPattern,
    },
    // Consumes an owned aggregate, moving each named field into its local
    Unpack {
        value: HMIRExprID,
        bindings: Vec<(CHLIdent, HMIRLocalID)>,
    },
}

#[derive(Debug, Clone)]
pub enum HMIRPattern {
    Wildcard,
    Binding(HMIRLocalID),
    Integer(i128),
    Float(FloatWrapper),
    // A comptime constant the subject is compared against
    Value(HMIRExprID),
    // The variant is looked up by name in the subject's type; 'sum' is the type the source
    // qualified it with, if any
    Variant {
        sum: Option<HMIRExprID>,
        name: CHLIdent,
        inner: Option<Box<HMIRPattern>>,
    },
}

impl HMIRAggregateOp {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Member { .. } => "op.member",
            Self::Index { .. } => "op.index",
            Self::Initialize { .. } => "op.initialize",
            Self::Is { .. } => "op.is",
            Self::Unpack { .. } => "op.unpack",
        }
    }
}
