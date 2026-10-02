use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::{binding::HMIRLocalID, expr::kind::HMIRExprID};

#[derive(Debug, Clone)]
pub enum HMIRAggregateOp {
    Member {
        base: HMIRExprID,
        name: CXIdent,
    },
    Index {
        base: HMIRExprID,
        index: HMIRExprID,
    },
    Initialize {
        ty: HMIRExprID,
        fields: Vec<(Option<CXIdent>, HMIRExprID)>,
    },
    Is {
        value: HMIRExprID,
        pattern: HMIRPattern,
    },
    // Consumes an owned aggregate, moving each named field into its local
    Unpack {
        value: HMIRExprID,
        bindings: Vec<(CXIdent, HMIRLocalID)>,
    },
}

#[derive(Debug, Clone)]
pub enum HMIRPattern {
    Binding(HMIRLocalID),
    Integer(i64),
    Float(FloatWrapper),
    Variant {
        sum: HMIRExprID,
        index: usize,
        inner: Option<HMIRLocalID>,
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
