use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::{
    binding::HMIRObjLocalID,
    def::HMIRDefRef,
    expr::{
        HMIRConstant, HMIRExprID,
        aggregate::{HMIRInitializer, HMIRPattern},
        intrinsic::HMIRObjIntrinsic,
        meta::HMIRMetaID,
        operator::{HMIRBinaryOp, HMIRCoerceMode, HMIRUnaryOp},
    },
};

dense_id!(HMIRObjID, "o");

#[derive(Debug, Clone)]
pub enum HMIRObjKind {
    Constant(HMIRConstant),
    Local(HMIRObjLocalID),
    Global(HMIRDefRef),
    FunctionAddress {
        function: HMIRMetaID,
        statics: Vec<HMIRMetaID>,
    },
    Lift(HMIRMetaID),
    Splice {
        quote: HMIRMetaID,
        args: Vec<HMIRObjID>,
    },

    Intrinsic(HMIRObjIntrinsic),
    Binary {
        op: HMIRBinaryOp,
        lhs: HMIRObjID,
        rhs: HMIRObjID,
    },
    Unary {
        op: HMIRUnaryOp,
        operand: HMIRObjID,
    },
    Coerce {
        mode: HMIRCoerceMode,
        from: HMIRMetaID,
        to: HMIRMetaID,
        value: HMIRObjID,
    },
    Retype {
        value: HMIRObjID,
        to: HMIRMetaID,
    },

    Let {
        local: HMIRObjLocalID,
        initializer: Option<HMIRObjID>,
    },
    Adopt {
        local: HMIRObjLocalID,
        value: HMIRObjID,
    },
    Move(HMIRObjLocalID),
    Initialize {
        ty: HMIRMetaID,
        initializer: HMIRInitializer,
    },

    Call {
        callee: HMIRMetaID,
        statics: Vec<HMIRMetaID>,
        args: Vec<HMIRObjID>,
    },
    CallIndirect {
        callee: HMIRObjID,
        args: Vec<HMIRObjID>,
    },

    Block {
        statements: Vec<HMIRExprID>,
        tail: Option<HMIRObjID>,
    },
    If {
        condition: HMIRExprID,
        then_branch: HMIRObjID,
        else_branch: Option<HMIRObjID>,
    },
    While {
        condition: HMIRExprID,
        body: HMIRObjID,
        pre_eval: bool,
    },
    For {
        init: HMIRExprID,
        condition: HMIRExprID,
        increment: HMIRExprID,
        body: HMIRObjID,
    },
    Switch {
        condition: HMIRExprID,
        cases: Vec<(HMIRMetaID, HMIRObjID)>,
        default: Option<HMIRObjID>,
    },
    Match {
        scrutinee: HMIRExprID,
        subject: HMIRObjLocalID,
        arms: Vec<(HMIRPattern, HMIRObjID)>,
    },
    Label {
        name: CXIdent,
        body: HMIRObjID,
    },
    Return(Option<HMIRObjID>),
    Yield(Option<HMIRObjID>),

    Error,
}

#[derive(Debug, Clone)]
pub struct HMIRObjExpr {
    kind: HMIRObjKind,
    span: TokenRange,
}

impl HMIRObjExpr {
    pub fn new(kind: HMIRObjKind, span: TokenRange) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &HMIRObjKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut HMIRObjKind {
        &mut self.kind
    }

    pub fn span(&self) -> &TokenRange {
        &self.span
    }
}
