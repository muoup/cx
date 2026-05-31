use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};
use speedy::{Readable, Writable};

use crate::mir::data::MIRFunctionPrototype;
use crate::mir::pattern::MIRPattern;
use crate::mir::r#type::{MIRFloatType, MIRIntegerType, MIRType, MIRTypeKind};

#[derive(Clone, Debug, Default)]
pub struct MIRFunctionContract {
    pub safe: bool,
    pub precondition: Option<Box<MIRExpression>>,
    pub postcondition: Option<(Option<CXIdent>, Box<MIRExpression>)>,
}

#[derive(Clone, Debug, Default)]
pub struct MIRExpression {
    pub kind: MIRExpressionKind,
    pub _type: MIRType,
    pub token_range: Option<TokenRange>,
}

#[derive(Clone, Debug)]
pub enum MIRPureExpression {
    IntegerLiteral(i64, MIRIntegerType, bool),
    FunctionReference(Box<MIRFunctionPrototype>),
}

impl MIRPureExpression {
    pub fn as_value(&self) -> MIRExpression {
        match self {
            Self::IntegerLiteral(value, integer_type, signed) => MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::IntLiteral(*value, *integer_type, *signed),
                _type: MIRType::from(MIRTypeKind::Integer {
                    _type: *integer_type,
                    signed: *signed,
                }),
            },
            Self::FunctionReference(prototype) => MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::FunctionReference {
                    name: prototype.name().clone(),
                },
                _type: MIRType::from(MIRTypeKind::Function {
                    signature: Box::new(prototype.signature().clone()),
                }),
            },
        }
    }
}

#[derive(Clone, Debug, Default, Readable, Writable)]
pub struct MIRSourceRange {
    pub start_token: usize,
    pub end_token: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolValueOrigin {
    Local,
    Global,
    Contract,
}

#[derive(Clone, Debug, Default)]
pub enum MIRExpressionKind {
    // Literals
    BoolLiteral(bool),
    IntLiteral(i64, MIRIntegerType, bool),
    FloatLiteral(FloatWrapper, MIRFloatType),

    #[default]
    Unit,

    // Variables
    Variable {
        name: CXIdent,
        location: SymbolValueOrigin,
    },

    ContractVariable {
        name: CXIdent,
        force_param: bool,
    },

    // The callable signature is stored in the expression's type
    FunctionReference {
        name: CXIdent,
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
    RegionCreate {
        _type: MIRType,
        initial_value: Option<Box<MIRExpression>>,
    },
    BindRegion {
        name: CXIdent,
        _type: MIRType,
        initial_region: Box<MIRExpression>,
        adopting: bool,
    },
    RegionDuplicate {
        source: Box<MIRExpression>,
    },
    ByValueArgument {
        source: Box<MIRExpression>,
    },
    RegionMove {
        source: Box<MIRExpression>,
    },
    RegionWrite {
        target: Box<MIRExpression>,
        value: Box<MIRExpression>,
    },

    // Represents a no-op used to change the type of an expression with no added semantics
    Typechange(Box<MIRExpression>),

    // Aggregate Access
    MemberAccess {
        base: Box<MIRExpression>,
        member_index: usize,
        aggregate_type: MIRType,
    },
    ArrayAccess {
        array: Box<MIRExpression>,
        index: Box<MIRExpression>,
        element_type: MIRType,
    },

    PatternIs {
        lhs: Box<MIRExpression>,
        pattern: MIRPattern,
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

    // Internal node used by generated type-constructor functions.
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
        initializations: Vec<StructInitialization>,
        struct_type: MIRType,
    },

    // Control Flow
    Break {
        scope_depth: usize,
    },
    Continue {
        scope_depth: usize,
    },
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
        arms: Vec<(MIRPattern, Box<MIRExpression>)>,
        default: Option<Box<MIRExpression>>,
        exhaustive: bool,
    },

    Return {
        postcondition: Option<(Option<CXIdent>, Box<MIRExpression>)>,
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
        contract: MIRFunctionContract,
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

    Unsafe {
        expression: Box<MIRExpression>,
    },
}

#[derive(Clone, Debug, Readable, Writable)]
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
    SHL,
    ASHR,
    LSHR,
}

#[derive(Clone, Debug, Readable, Writable)]
pub enum MIRPtrDiffBinOp {
    ADD,
    SUB,
}

#[derive(Clone, Debug, Readable, Writable)]
pub enum MIRPtrBinOp {
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
}

#[derive(Clone, Debug, Readable, Writable)]
pub enum MIRFloatBinOp {
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
pub enum MIRBinOp {
    Integer {
        itype: MIRIntegerType,
        op: MIRIntegerBinOp,
    },

    Float {
        ftype: MIRFloatType,
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

#[derive(Clone, Debug, Readable, Writable)]
pub enum MIRUnOp {
    NEG,
    INEG,
    FNEG,
    BNOT,
    LNOT,

    PreIncrement(i8),
    PostIncrement(i8),
}

#[derive(Clone, Copy, Debug, Readable, Writable)]
pub enum MIRCoercion {
    // Any integer to any integer conversion
    Integral {
        sextend: bool,
        from_type: MIRIntegerType,
        to_type: MIRIntegerType,
    },

    // Any float to any float conversion
    FloatCast {
        to_type: MIRFloatType,
    },

    // Any integer type to a floating point number, sizes of types need not match
    IntToFloat {
        to_type: MIRFloatType,
        sextend: bool,
    },

    // Any float type to any integer type, sizes of types need not match
    FloatToInt {
        to_type: MIRIntegerType,
        sextend: bool,
    },

    // Pointer to any specified integer type
    PtrToInt {
        to_type: MIRIntegerType,
    },

    // Any sized integer type to a pointer
    IntToPtr {
        sextend: bool,
    },

    // Decay of function designator to a pointer value
    GetFnPtr,

    // Conversions between types that have the same semantic meaning
    // in assembly, this is typically a no-op, but proves useful for type checking and verification
    Typechange,

    // A similar no-op operation like Typechange, but represents conversions that *do* change the semantic
    // meaning of the bits, such as converting from an f32 to an i32
    ReinterpretBits,
}

#[derive(Clone, Debug)]
pub struct StructInitialization {
    pub field_index: usize,
    pub value: MIRExpression,
}

impl MIRExpression {
    pub fn get_type(&self) -> MIRType {
        self._type.clone()
    }

    pub fn int_literal(value: i64, itype: MIRIntegerType, is_signed: bool) -> Self {
        Self {
            kind: MIRExpressionKind::IntLiteral(value, itype, is_signed),
            _type: MIRType {
                kind: MIRTypeKind::Integer {
                    _type: itype,
                    signed: is_signed,
                },

                ..Default::default()
            },
            token_range: None,
        }
    }
}
