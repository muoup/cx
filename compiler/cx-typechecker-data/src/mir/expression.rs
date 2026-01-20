use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::mir::types::{CXFloatType, CXIntegerType, MIRType, MIRTypeKind};

#[derive(Clone, Debug, Default)]
pub struct MIRExpression {
    pub kind: MIRExpressionKind,
    pub _type: MIRType,
}

#[derive(Clone, Debug, Default)]
pub enum MIRExpressionKind {
    // Literals
    BoolLiteral(bool),
    IntLiteral(i64, CXIntegerType, bool),
    FloatLiteral(FloatWrapper, CXFloatType),
    Null,

    #[default]
    Unit,

    // Variables
    Variable(CXIdent),

    // The prototype is implicitly stored in the expression's type
    FunctionReference {
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
    },
    MemoryWrite {
        target: Box<MIRExpression>,
        value: Box<MIRExpression>,
    },
    CreateStackVariable {
        name: Option<CXIdent>,
        _type: MIRType,
    },
    CopyRegion {
        source: Box<MIRExpression>,
        _type: MIRType,
    },

    // Represents a no-op used to change the type of an expression with no added semantics
    Typechange(Box<MIRExpression>),
    
    // Aggregate Access
    StructFieldAccess {
        base: Box<MIRExpression>,
        field_index: usize,
        field_offset: usize,
        struct_type: MIRType,
    },
    UnionAliasAccess {
        base: Box<MIRExpression>,
        variant_type: MIRType,
        union_type: MIRType,
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
    TaggedUnionSet {
        target: Box<MIRExpression>,
        variant_index: usize,
        inner_value: Box<MIRExpression>,
        sum_type: MIRType,
    },
    
    ConstructTaggedUnion {
        variant_index: usize,
        value: Box<MIRExpression>,
        sum_type: MIRType,
    },
    
    ArrayInitializer {
        elements: Vec<MIRExpression>,
        element_type: MIRType,
    },
    StructInitializer {
        initializations: Vec<(usize, MIRExpression)>,
        struct_type: MIRType,
    },

    // Control Flow
    Break,
    Continue,
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
    
    CSwitch {
        condition: Box<MIRExpression>,
        cases: Vec<(Box<MIRExpression>, Box<MIRExpression>)>,
        default: Option<Box<MIRExpression>>,
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

    // Logical Ops
    LAND,
    LOR,
    
    // Boolean/Bitwise Ops
    BAND,
    BOR,
    BXOR,
}

#[derive(Clone, Debug)]
pub enum MIRPtrDiffBinOp {
    ADD,
    SUB,
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
    
    PreIncrement(i8),
    PostIncrement(i8),
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

impl MIRExpression {
    pub fn get_type(&self) -> MIRType {
        self._type.clone()
    }

    pub fn int_literal(value: i64, itype: CXIntegerType, is_signed: bool) -> Self {
        Self {
            kind: MIRExpressionKind::IntLiteral(value, itype, is_signed),
            _type: MIRType {
                kind: MIRTypeKind::Integer {
                    _type: itype,
                    signed: is_signed,
                },
                visibility: Default::default(),
                specifiers: 0,
            },
        }
    }
}
