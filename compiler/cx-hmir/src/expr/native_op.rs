use cx_util::identifier::CXIdent;

use crate::{
    expr::{aggregate::HMIRAggregateOp, kind::HMIRExprID, type_op::HMIRTypeOp},
    unit::def::HMIRDefRef,
};

#[derive(Debug, Clone)]
pub enum HMIRNativeOp {
    BinOp {
        op: HMIRBinaryOp,
        lhs: HMIRExprID,
        rhs: HMIRExprID,
    },
    UnOp {
        op: HMIRUnaryOp,
        operand: HMIRExprID,
    },
    Coerce {
        mode: HMIRCoerceMode,
        value: HMIRExprID,
        target: HMIRExprID,
    },
    Assign {
        target: HMIRExprID,
        op: Option<HMIRBinaryOp>,
        value: HMIRExprID,
    },
    AddressOf(HMIRExprID),
    Dereference(HMIRExprID),

    Type(HMIRTypeOp),
    Control(HMIRControlOp),
    OwnershipOp(HMIROwnershipOp),
    AggregateOp(HMIRAggregateOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,

    LAnd,
    LOr,
    BAnd,
    BOr,
    BXor,

    LShift,
    RShift,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRUnaryOp {
    Neg,
    LNot,
    BNot,
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRCoerceMode {
    Implicit,
    Convert,
    CCast,
    Truthy,
}

#[derive(Debug, Clone)]
pub enum HMIRControlOp {
    Return(Option<HMIRExprID>),
    Yield(Option<HMIRExprID>),
    Break,
    Continue,
    Goto(CXIdent),
    // Jumps to the label whose address the operand holds
    IndirectGoto(HMIRExprID),
    // 'function' owns the label; a function-level static takes the address outside of its body
    LabelAddress {
        function: Option<HMIRDefRef>,
        name: CXIdent,
    },

    Defer(HMIRExprID),
    Unsafe(HMIRExprID),
    Unreachable,
}

#[derive(Debug, Clone)]
pub enum HMIROwnershipOp {
    Allocate(HMIRExprID),
    Adopt(HMIRExprID),
    Leak(HMIRExprID),
    Move(HMIRExprID),
}

impl HMIRBinaryOp {
    pub fn path(self) -> &'static str {
        match self {
            Self::Add => "op.add",
            Self::Sub => "op.sub",
            Self::Mul => "op.mul",
            Self::Div => "op.div",
            Self::Mod => "op.mod",
            Self::Eq => "op.eq",
            Self::Neq => "op.neq",
            Self::Lt => "op.lt",
            Self::Le => "op.le",
            Self::Gt => "op.gt",
            Self::Ge => "op.ge",
            Self::LAnd => "op.l_and",
            Self::LOr => "op.l_or",
            Self::BAnd => "op.b_and",
            Self::BOr => "op.b_or",
            Self::BXor => "op.b_xor",
            Self::LShift => "op.l_shift",
            Self::RShift => "op.r_shift",
        }
    }
}

impl HMIRUnaryOp {
    pub fn path(self) -> &'static str {
        match self {
            Self::Neg => "op.neg",
            Self::LNot => "op.l_not",
            Self::BNot => "op.b_not",
            Self::PreIncrement => "op.pre_inc",
            Self::PreDecrement => "op.pre_dec",
            Self::PostIncrement => "op.post_inc",
            Self::PostDecrement => "op.post_dec",
        }
    }
}

impl HMIRCoerceMode {
    pub fn path(self) -> &'static str {
        match self {
            Self::Implicit => "op.coerce",
            Self::Convert => "op.convert",
            Self::CCast => "op.c_cast",
            Self::Truthy => "op.truthy",
        }
    }
}

impl HMIRControlOp {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Return(_) => "return",
            Self::Yield(_) => "yield",
            Self::Break => "break",
            Self::Continue => "continue",
            Self::Goto(_) => "goto",
            Self::IndirectGoto(_) => "goto *",
            Self::LabelAddress { .. } => "label_address",
            Self::Defer(_) => "defer",
            Self::Unsafe(_) => "unsafe",
            Self::Unreachable => "unreachable",
        }
    }
}

impl HMIROwnershipOp {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Allocate(_) => "allocate",
            Self::Adopt(_) => "adopt",
            Self::Leak(_) => "leak",
            Self::Move(_) => "move",
        }
    }
}
