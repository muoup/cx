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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRCoerceMode {
    Implicit,
    Convert,
    CCast,
    Truthy,
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
