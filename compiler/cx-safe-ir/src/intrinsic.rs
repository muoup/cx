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
    NEG,
    INEG,
    FNEG,
    BNOT,
    LNOT,
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
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    IMUL,
    IDIV,
    IMOD,
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
    ILT,
    ILE,
    IGT,
    IGE,
    LAND,
    LOR,
    BAND,
    BOR,
    BXOR,
}

#[derive(Clone, Debug)]
pub enum FMIRIntrinsicFBinOp {
    FADD,
    FSUB,
    FMUL,
    FDIV,
    FEQ,
    FNE,
    FLT,
    FLE,
    FGT,
    FGE,
}

#[derive(Clone, Debug)]
pub enum FMIRPointerBinaryIntrinsicOp {
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
}

#[derive(Clone, Debug)]
pub enum FMIRPointerDiffBinaryIntrinsicOp {
    ADD,
    SUB,
}