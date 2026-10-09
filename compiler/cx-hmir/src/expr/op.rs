use cx_util::identifier::CXIdent;

use crate::{
    expr::{HMIRExprID, aggregate::HMIRAggregateOp},
    unit::def::HMIRDefRef,
};

#[derive(Debug, Clone)]
pub enum HMIROp {
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

#[derive(Debug, Clone)]
pub enum HMIRTypeOp {
    Pointer(HMIRExprID),
    Reference(HMIRExprID),
    Const(HMIRExprID),
    PointerInner(HMIRExprID),
    ReferenceInner(HMIRExprID),
    Member {
        ty: HMIRExprID,
        name: CXIdent,
    },
    TypeOf(HMIRExprID),
    Decay(HMIRExprID),
    Array {
        element: HMIRExprID,
        length: Option<HMIRExprID>,
    },
    Function {
        params: Vec<HMIRExprID>,
        ret: HMIRExprID,
        variadic: bool,
    },
    Expr {
        params: Vec<HMIRExprID>,
        result: HMIRExprID,
    },
    Aggregate {
        ty: HMIRExprID,
        pairs: Vec<(Option<CXIdent>, HMIRExprID)>,
    },

    SizeOf(HMIRExprID),
    AlignOf(HMIRExprID),
    // The offset in bytes of the subobject of 'ty' that 'member' leads to
    OffsetOf {
        ty: HMIRExprID,
    },
    IsInt(HMIRExprID),
    IsFloat(HMIRExprID),
    IsPointer(HMIRExprID),
    IsSigned(HMIRExprID),
    Equal(HMIRExprID, HMIRExprID),
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

impl HMIRTypeOp {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Pointer(_) => "type.pointer",
            Self::Reference(_) => "type.reference",
            Self::Const(_) => "type.const",
            Self::PointerInner(_) => "type.pointer_inner",
            Self::ReferenceInner(_) => "type.reference_inner",
            Self::Member { .. } => "type.member",
            Self::TypeOf(_) => "type.type_of",
            Self::Decay(_) => "type.decay",
            Self::Array { .. } => "type.array",
            Self::Function { .. } => "type.function",
            Self::Expr { .. } => "type.expr",
            Self::Aggregate { .. } => "type.aggregate",
            Self::SizeOf(_) => "type.size_of",
            Self::AlignOf(_) => "type.align_of",
            Self::OffsetOf { .. } => "type.offset_of",
            Self::IsInt(_) => "type.is_int",
            Self::IsFloat(_) => "type.is_float",
            Self::IsPointer(_) => "type.is_pointer",
            Self::IsSigned(_) => "type.is_signed",
            Self::Equal(..) => "type.equal",
        }
    }
}
