use cx_intrinsics::Intrinsic;
use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::{HMIRDefRef, HMIRHoleID, HMIRLocalID, HMIROp, HMIRPattern, expr::constant::HMIRConstant};

pub mod aggregate;
pub mod constant;
pub mod op;

dense_id!(HMIRExprID, "e");

pub type HMIRIntrinsic = Intrinsic<HMIRExprID, HMIRExprID>;

#[derive(Debug, Clone)]
pub struct HMIRExpr {
    kind: HMIRExprKind,
    span: TokenRange,
}

#[derive(Debug, Clone)]
pub enum HMIRExprKind {
    Constant(HMIRConstant),
    Local(HMIRLocalID),
    Def(HMIRDefRef),
    Hole(HMIRHoleID),

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
    Op(HMIROp),
    Error(CXIdent),

    Let {
        local: HMIRLocalID,
        initializer: Option<HMIRExprID>,
    },
    Call {
        callee: HMIRExprID,
        args: Vec<HMIRExprID>,
    },

    Block {
        statements: Vec<HMIRExprID>,
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
        body: HMIRExprID,
    },
    // A label of the innermost enclosing switch; 'default' when it has no value
    Case {
        value: Option<HMIRExprID>,
        body: HMIRExprID,
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