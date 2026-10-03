use cx_intrinsics::Intrinsic;
use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::{
    binding::{HMIRHoleID, HMIRLocalID},
    expr::{HMIRConstant, aggregate::HMIRPattern, native_op::HMIRNativeOp},
    unit::def::HMIRDefRef,
};

dense_id!(HMIRExprID, "e");

pub type HMIRIntrinsic = Intrinsic<HMIRExprID, HMIRExprID>;

// A diagnostic found while lowering into HMIR, reported once staging reaches its expression
#[derive(Debug, Clone)]
pub struct HMIRError {
    pub code: &'static str,
    pub message: String,
}

#[derive(Debug, Clone)]
pub enum HMIRExprKind {
    Constant(HMIRConstant),
    Local(HMIRLocalID),
    Def(HMIRDefRef),
    Hole(HMIRHoleID),
    Error(HMIRError),

    Comptime(HMIRExprID),
    Quote {
        params: Vec<HMIRLocalID>,
        body: HMIRExprID,
    },
    Splice {
        quote: HMIRExprID,
        args: Vec<HMIRExprID>,
    },

    Intrinsic(HMIRIntrinsic),
    Native(HMIRNativeOp),

    Let {
        local: HMIRLocalID,
        initializer: Option<HMIRExprID>,
    },
    Call {
        callee: HMIRExprID,
        args: Vec<HMIRExprID>,
    },

    Block {
        kind: HMIRBlockKind,
        statements: Vec<HMIRExprID>,
        tail: Option<HMIRExprID>,
    },
    If {
        condition: HMIRExprID,
        then_branch: HMIRExprID,
        else_branch: Option<HMIRExprID>,
    },
    While {
        condition: HMIRExprID,
        body: HMIRExprID,
        pre_eval: bool,
    },
    For {
        init: HMIRExprID,
        condition: HMIRExprID,
        increment: HMIRExprID,
        body: HMIRExprID,
    },
    Switch {
        condition: HMIRExprID,
        cases: Vec<(HMIRExprID, HMIRExprID)>,
        default: Option<HMIRExprID>,
    },
    Match {
        scrutinee: HMIRExprID,
        subject: HMIRLocalID,
        arms: Vec<(HMIRPattern, HMIRExprID)>,
    },
    Label {
        name: CXIdent,
        body: HMIRExprID,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRBlockKind {
    Sequence,
    Scope,
    Yield,
}

#[derive(Debug, Clone)]
pub struct HMIRExpr {
    kind: HMIRExprKind,
    span: TokenRange,
}

impl HMIRExpr {
    pub fn new(kind: HMIRExprKind, span: TokenRange) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &HMIRExprKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut HMIRExprKind {
        &mut self.kind
    }

    pub fn span(&self) -> &TokenRange {
        &self.span
    }
}
