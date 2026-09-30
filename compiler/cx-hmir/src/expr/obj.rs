use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    HMIRObjIntrinsic,
    constant::HMIRConstant,
    def::HMIRDefRef,
    expr::{
        aggregate::HMIRInitializer,
        operator::{HMIRBinaryOp, HMIRCoerceMode, HMIRUnaryOp},
        pattern::HMIRPattern,
    },
    ids::{HMIRMetaID, HMIRMetaLocalID, HMIRObjID, HMIRObjLocalID},
};

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
    Increment {
        target: HMIRObjID,
        amount: i8,
        postfix: bool,
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
    StaticLet {
        local: HMIRMetaLocalID,
        initializer: HMIRMetaID,
    },
    Load(HMIRObjID),
    Move(HMIRObjLocalID),
    Assign {
        target: HMIRObjID,
        value: HMIRObjID,
    },
    AddressOf(HMIRObjID),
    Dereference(HMIRObjID),

    Member {
        base: HMIRObjID,
        name: CXIdent,
    },
    Field {
        base: HMIRObjID,
        index: usize,
        aggregate: HMIRMetaID,
    },
    Index {
        base: HMIRObjID,
        index: HMIRObjID,
        element: HMIRMetaID,
    },
    Initialize {
        ty: HMIRMetaID,
        initializer: HMIRInitializer,
    },
    Tag {
        value: HMIRObjID,
        sum: HMIRMetaID,
    },
    SetVariant {
        target: HMIRObjID,
        index: usize,
        value: HMIRObjID,
        sum: HMIRMetaID,
    },
    Is {
        value: HMIRObjID,
        pattern: HMIRPattern,
    },
    Unpack {
        value: HMIRObjID,
        bindings: Vec<(usize, HMIRObjLocalID)>,
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
        statements: Vec<HMIRObjID>,
        tail: Option<HMIRObjID>,
    },
    If {
        condition: HMIRObjID,
        then_branch: HMIRObjID,
        else_branch: Option<HMIRObjID>,
    },
    StaticIf {
        condition: HMIRMetaID,
        then_branch: HMIRObjID,
        else_branch: Option<HMIRObjID>,
    },
    While {
        condition: HMIRObjID,
        body: HMIRObjID,
        pre_eval: bool,
    },
    For {
        init: HMIRObjID,
        condition: HMIRObjID,
        increment: HMIRObjID,
        body: HMIRObjID,
    },
    Switch {
        condition: HMIRObjID,
        cases: Vec<(HMIRMetaID, HMIRObjID)>,
        default: Option<HMIRObjID>,
    },
    Match {
        scrutinee: HMIRObjID,
        subject: HMIRObjLocalID,
        arms: Vec<(HMIRPattern, HMIRObjID)>,
    },
    Break,
    Continue,
    Goto(CXIdent),
    Label {
        name: CXIdent,
        body: HMIRObjID,
    },
    Return(Option<HMIRObjID>),
    Yield(Option<HMIRObjID>),
    Unreachable,

    Defer(HMIRObjID),
    Leak(HMIRObjID),
    Unsafe(HMIRObjID),

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
