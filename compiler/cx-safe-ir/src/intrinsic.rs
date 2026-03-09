#[derive(Clone, Debug)]
pub struct FMIRIntrinsicFunction {
    pub kind: FMIRIntrinsicKind,
}

#[derive(Clone, Debug)]
pub enum FMIRIntrinsicKind {
    Unary(FMIRUnaryIntrinsic),
    Binary(FMIRBinaryIntrinsic),
    Cast(FMIRCastIntrinsic),
}

#[derive(Clone, Debug)]
pub enum FMIRCastIntrinsic {
    Integral {
        sextend: bool,
        to_bits: usize,
    },
    FloatCast {
        to_bits: usize,
    },
    PtrToInt {
        to_bits: usize,
    },
    IntToPtr {
        sextend: bool,
    },
    IntToFloat {
        to_bits: usize,
        sextend: bool,
    },
    FloatToInt {
        to_bits: usize,
        sextend: bool,
    },
    IntToBool,
    ReinterpretBits,
}

#[derive(Clone, Debug)]
pub enum FMIRUnaryIntrinsic {
    Neg,
    INeg,
    FNeg,
    BNot,
    LNot,
}

#[derive(Clone, Debug)]
pub enum FMIRBinaryIntrinsic {
    Integer {
        bits: usize,
        op: FMIRIntrinsicIBinOp,
    },
    Float {
        bits: usize,
        op: FMIRIntrinsicFBinOp,
    },
    Pointer {
        op: FMIRPointerBinaryIntrinsicOp,
    },
    PointerDiff {
        op: FMIRPointerDiffBinaryIntrinsicOp,
    },
}

#[derive(Clone, Debug)]
pub enum FMIRIntrinsicIBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    IMul,
    IDiv,
    IMod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    ILt,
    ILe,
    IGt,
    IGe,
    LAnd,
    LOr,
    BAnd,
    BOr,
    BXor,
}

#[derive(Clone, Debug)]
pub enum FMIRIntrinsicFBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Clone, Debug)]
pub enum FMIRPointerBinaryIntrinsicOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Clone, Debug)]
pub enum FMIRPointerDiffBinaryIntrinsicOp {
    Add,
    Sub,
}