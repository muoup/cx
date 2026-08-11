use cx_thir::thir::r#type::{THIRFloatType, THIRIntType};

use crate::MIRType;

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
        ty: THIRIntType,
        signed: bool,
        op: MIRIntBinaryOp,
    },
    Float {
        ty: THIRFloatType,
        op: MIRFloatBinaryOp,
    },
    PointerOffset {
        op: MIRPointerOffsetOp,
        pointee: Box<MIRType>,
    },
    Pointer(MIRPointerBinaryOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRUnaryOp {
    IntegerNeg { ty: THIRIntType, signed: bool },
    FloatNeg(THIRFloatType),
    BitNot(THIRIntType),
    LogicalNot,
    Increment { amount: i8, post: bool },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRCoercion {
    Integral {
        sign_extend: bool,
        from: THIRIntType,
        to: THIRIntType,
    },
    FloatCast {
        from: THIRFloatType,
        to: THIRFloatType,
    },
    IntToFloat {
        from: THIRIntType,
        to: THIRFloatType,
        signed: bool,
    },
    FloatToInt {
        from: THIRFloatType,
        to: THIRIntType,
        signed: bool,
    },
    PointerToInt {
        to: THIRIntType,
    },
    IntToPointer {
        from: THIRIntType,
        sign_extend: bool,
    },
    FunctionToPointer,
    TypeChange,
    ReinterpretBits,
}
