use crate::ty::{MIRFloatType, MIRIntType, MIRTypeID};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRIntBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    SignedMul,
    SignedDiv,
    SignedMod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    SignedLt,
    SignedLe,
    SignedGt,
    SignedGe,
    LogicalAnd,
    LogicalOr,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ArithmeticShiftRight,
    LogicalShiftRight,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRFloatBinaryOp {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRPointerOffsetOp {
    Add,
    Sub,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRPointerBinaryOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
pub enum MIRBinaryOp {
    Integer {
        ty: MIRIntType,
        signed: bool,
        op: MIRIntBinaryOp,
    },
    Float {
        ty: MIRFloatType,
        op: MIRFloatBinaryOp,
    },
    PointerOffset {
        op: MIRPointerOffsetOp,
        pointee: MIRTypeID,
    },
    Pointer(MIRPointerBinaryOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRUnaryOp {
    IntegerNeg { ty: MIRIntType, signed: bool },
    FloatNeg(MIRFloatType),
    BitNot(MIRIntType),
    LogicalNot,
    Increment { amount: i8, post: bool },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRCoercion {
    Integral {
        sign_extend: bool,
        from: MIRIntType,
        to: MIRIntType,
    },
    FloatCast {
        from: MIRFloatType,
        to: MIRFloatType,
    },
    IntToFloat {
        from: MIRIntType,
        to: MIRFloatType,
        signed: bool,
    },
    FloatToInt {
        from: MIRFloatType,
        to: MIRIntType,
        signed: bool,
    },
    PointerToInt {
        to: MIRIntType,
    },
    IntToPointer {
        from: MIRIntType,
        sign_extend: bool,
    },
    FunctionToPointer,
    TypeChange,
    ReinterpretBits,
}
