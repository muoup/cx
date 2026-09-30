use cx_tokens::TokenRange;
use cx_util::dense_id;

use crate::{
    binding::{HMIRHoleID, HMIRMetaLocalID, HMIRObjLocalID},
    def::HMIRDefRef,
    expr::{
        HMIRConstant,
        intrinsic::HMIRMetaIntrinsic,
        obj::HMIRObjID,
        operator::{HMIRBinaryOp, HMIRCoerceMode, HMIRUnaryOp},
        type_op::HMIRTypeOp,
    },
};

dense_id!(HMIRMetaID, "m");

#[derive(Debug, Clone)]
pub enum HMIRMetaKind {
    Constant(HMIRConstant),
    Local(HMIRMetaLocalID),
    Def(HMIRDefRef),
    Hole(HMIRHoleID),

    Intrinsic(HMIRMetaIntrinsic),
    TypeOp(HMIRTypeOp),
    Binary {
        op: HMIRBinaryOp,
        lhs: HMIRMetaID,
        rhs: HMIRMetaID,
    },
    Unary {
        op: HMIRUnaryOp,
        operand: HMIRMetaID,
    },
    Coerce {
        mode: HMIRCoerceMode,
        from: HMIRMetaID,
        to: HMIRMetaID,
        value: HMIRMetaID,
    },

    Call {
        callee: HMIRMetaID,
        args: Vec<HMIRMetaID>,
    },

    Let {
        local: HMIRMetaLocalID,
        initializer: Option<HMIRMetaID>,
    },
    Assign {
        local: HMIRMetaLocalID,
        value: HMIRMetaID,
    },

    Block {
        statements: Vec<HMIRMetaID>,
        tail: Option<HMIRMetaID>,
    },
    If {
        condition: HMIRMetaID,
        then_branch: HMIRMetaID,
        else_branch: Option<HMIRMetaID>,
    },
    While {
        condition: HMIRMetaID,
        body: HMIRMetaID,
    },
    Return(Option<HMIRMetaID>),

    Quote {
        params: Vec<HMIRObjLocalID>,
        body: HMIRObjID,
    },
    Error,
}

#[derive(Debug, Clone)]
pub struct HMIRMetaExpr {
    kind: HMIRMetaKind,
    span: TokenRange,
}

impl HMIRMetaExpr {
    pub fn new(kind: HMIRMetaKind, span: TokenRange) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &HMIRMetaKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut HMIRMetaKind {
        &mut self.kind
    }

    pub fn span(&self) -> &TokenRange {
        &self.span
    }
}
