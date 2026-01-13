use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::mir::types::{CXFloatType, CXIntegerType, MIRFunctionPrototype, MIRType};

#[derive(Clone, Debug)]
pub struct MIRExpression {
    pub kind: MIRExpressionKind,
    pub _type: MIRType,
}

#[derive(Clone, Debug)]
pub enum MIRExpressionKind {
    // Literals
    BoolLiteral(bool),
    IntLiteral(i64, CXIntegerType, bool),
    FloatLiteral(FloatWrapper, CXFloatType),
    Null,
    Unit,

    // Variables
    Parameter(CXIdent),
    GlobalVariable(CXIdent),
    LocalVariable(CXIdent),
    FunctionReference {
        prototype: MIRFunctionPrototype,
        implicit_variables: Vec<MIRExpression>,
    },

    // Arithmetic & Logic
    BinaryOperation {
        lhs: Box<MIRExpression>,
        rhs: Box<MIRExpression>,
        op: MIRBinOp,
    },
    UnaryOperation {
        operand: Box<MIRExpression>,
        op: MIRUnOp,
    },

    // Memory Operations
    Move {
        source: Box<MIRExpression>,
    },
    MemoryRead {
        source: Box<MIRExpression>,
        _type: MIRType,
    },
    MemoryWrite {
        target: Box<MIRExpression>,
        value: Box<MIRExpression>,
    },
    StackAllocation {
        _type: MIRType,
    },
    CopyRegion {
        source: Box<MIRExpression>,
        _type: MIRType,
    },

    // Aggregate Access
    StructFieldAccess {
        base: Box<MIRExpression>,
        field_index: usize,
        field_offset: usize,
        struct_type: MIRType,
    },
    ArrayAccess {
        array: Box<MIRExpression>,
        index: Box<MIRExpression>,
        element_type: MIRType,
    },

    // Tagged Unions
    TaggedUnionTag {
        value: Box<MIRExpression>,
        sum_type: MIRType,
    },
    TaggedUnionGet {
        value: Box<MIRExpression>,
        variant_type: MIRType,
    },
    ConstructTaggedUnion {
        variant_index: usize,
        value: Box<MIRExpression>,
        sum_type: MIRType,
    },

    // Control Flow
    If {
        condition: Box<MIRExpression>,
        then_branch: Box<MIRExpression>,
        else_branch: Option<Box<MIRExpression>>,
    },
    While {
        condition: Box<MIRExpression>,
        body: Box<MIRExpression>,
        pre_eval: bool,
    },
    For {
        init: Box<MIRExpression>,
        condition: Box<MIRExpression>,
        increment: Box<MIRExpression>,
        body: Box<MIRExpression>,
    },
    Match {
        condition: Box<MIRExpression>,
        arms: Vec<(Box<MIRExpression>, Box<MIRExpression>)>,
        default: Option<Box<MIRExpression>>,
    },
    Return {
        value: Option<Box<MIRExpression>>,
    },

    // Sequential Statements
    Block {
        statements: Vec<MIRExpression>,
    },

    // Function Calls
    CallFunction {
        function: Box<MIRExpression>,
        arguments: Vec<MIRExpression>,
    },

    // Type Conversion
    TypeConversion {
        operand: Box<MIRExpression>,
        conversion: MIRCoercion,
    },

    // Lifetime Management
    LifetimeStart {
        variable: CXIdent,
        _type: MIRType,
    },
    LifetimeEnd {
        variable: CXIdent,
        _type: MIRType,
    },
    LeakLifetime {
        expression: Box<MIRExpression>,
    },

    // Defer (TODO: Refactor to proper scoped chains)
    Defer {
        expression: Box<MIRExpression>,
    },
}

#[derive(Clone, Debug)]
pub enum MIRIntegerBinOp {
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
}

#[derive(Clone, Debug)]
pub enum MIRPtrDiffBinOp {
    ADD,
    SUB,
}

#[derive(Clone, Debug)]
pub enum MIRBoolBinOp {
    LAND,
    LOR,

    EQ,
    NE,
}

#[derive(Clone, Debug)]
pub enum MIRPtrBinOp {
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
}

#[derive(Clone, Debug)]
pub enum MIRFloatBinOp {
    FADD,
    FSUB,
    FMUL,
    FDIV,

    EQ,
    NEQ,

    FLT,
    FLE,
    FGT,
    FGE,
}

#[derive(Clone, Debug)]
pub enum MIRBinOp {
    Integer {
        itype: CXIntegerType,
        op: MIRIntegerBinOp,
    },

    Bool {
        op: MIRBoolBinOp,
    },

    Float {
        ftype: CXFloatType,
        op: MIRFloatBinOp,
    },

    /**
     *  Any binary operation instruction of this type must have the pointer value as the lhs, and the integer value as the rhs.
     */
    PtrDiff {
        op: MIRPtrDiffBinOp,

        // Boxed for size reasons
        ptr_inner: Box<MIRType>,
    },

    Pointer {
        op: MIRPtrBinOp,
    },
}

#[derive(Clone, Debug)]
pub enum MIRUnOp {
    NEG,
    INEG,
    FNEG,
    BNOT,
    LNOT,
}

#[derive(Clone, Copy, Debug)]
pub enum MIRCoercion {
    // Any integer to any integer conversion
    Integral {
        sextend: bool,
        to_type: CXIntegerType,
    },

    // Any float to any float conversion
    FloatCast {
        to_type: CXFloatType,
    },

    // Boolean (i1) to any integer type. This exists mostly for Cranelift, as internally a boolean
    // is simply a byte, and doing an integral cast from Bool -> i8 would create a coercion instruction
    // that Cranelift cannot handle. There could have been a filter for this, but that ended up causing
    // proving more difficult than just having a dedicated coercion type for it.
    BoolToInt {
        to_type: CXIntegerType,
    },

    // Any integer type to a floating point number, sizes of types need not match
    IntToFloat {
        to_type: CXFloatType,
        sextend: bool,
    },

    // Any float type to any integer type, sizes of types need not match
    FloatToInt {
        to_type: CXIntegerType,
        sextend: bool,
    },

    // Pointer to any specified integer type
    PtrToInt {
        to_type: CXIntegerType,
    },

    // Any sized integer type to a pointer
    IntToPtr {
        sextend: bool,
    },

    // Any integer type to a boolean (i1)
    IntToBool,

    // Conversions between equally sized types that do not change the bit representation,
    // in assembly, this is typically a no-op, but proves useful for type checking and verification
    ReinterpretBits,
}
