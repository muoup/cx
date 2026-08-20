use std::cell::Cell;

use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};
use speedy::{Readable, Writable};

use crate::thir::pattern::THIRPattern;
use crate::thir::r#type::{THIRFloatType, THIRIntType, THIRType, THIRTypeKind};

thread_local! {
    static NEXT_LOCAL_ID: Cell<u64> = const { Cell::new(0) };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct THIRLocalID(pub u64);

impl THIRLocalID {
    pub fn fresh() -> Self {
        NEXT_LOCAL_ID.with(|next| {
            let id = next.get();
            next.set(id.checked_add(1).expect("THIR local id counter overflowed"));
            Self(id)
        })
    }
}

#[derive(Clone, Debug, Default)]
pub struct THIRFnContract {
    pub safe: bool,
    pub noreturn: bool,
    pub precondition: Option<Box<THIRExpression>>,
    pub postcondition: Option<THIRPostcondition>,
}

#[derive(Clone, Debug)]
pub struct THIRPostcondition {
    pub binding: Option<CXIdent>,
    pub condition: Box<THIRExpression>,
}

#[derive(Clone, Debug)]
pub struct THIRExpression {
    pub kind: THIRExpressionKind,
    pub _type: THIRType,
    pub token_range: TokenRange,
}

impl Default for THIRExpression {
    fn default() -> Self {
        Self {
            kind: THIRExpressionKind::default(),
            _type: THIRType::default(),
            token_range: TokenRange::internal(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum THIRPureExpression {
    IntegerLiteral(i64, THIRIntType, bool),
}

impl THIRPureExpression {
    pub fn as_value(&self) -> THIRExpression {
        match self {
            Self::IntegerLiteral(value, integer_type, signed) => THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::IntLiteral(*value),
                _type: THIRType::from(THIRTypeKind::Integer {
                    _type: *integer_type,
                    signed: *signed,
                }),
            },
        }
    }
}

#[derive(Clone, Debug, Default, Readable, Writable)]
pub struct THIRSourceRange {
    pub start_token: usize,
    pub end_token: usize,
}

#[derive(Clone, Debug, Default)]
pub enum THIRExpressionKind {
    // Literals
    BoolLiteral(bool),
    IntLiteral(i64),
    FloatLiteral(FloatWrapper),
    StringLiteral {
        value: String,
    },

    #[default]
    Unit,

    // Variables
    GlobalVariable {
        symbol: CXIdent,
    },

    Variable {
        name: CXIdent,
        local_id: THIRLocalID,
    },

    ContractVariable {
        name: CXIdent,
        force_param: bool,
    },

    FunctionReference {
        name: CXIdent,
        debug_name: Option<CXIdent>,
    },
    SizeOf {
        _type: THIRType,
    },
    AlignOf {
        _type: THIRType,
    },

    // Arithmetic & Logic
    BinaryOperation {
        lhs: Box<THIRExpression>,
        rhs: Box<THIRExpression>,
        op: THIRBinOp,
    },
    UnaryOperation {
        operand: Box<THIRExpression>,
        op: THIRUnOp,
    },

    // Memory Operations
    CreateLocalVariable {
        name: CXIdent,
        local_id: THIRLocalID,
        _type: THIRType,
        initial_value: Option<Box<THIRExpression>>,
        adopting: bool,
    },
    Copy {
        source: Box<THIRExpression>,
    },
    Move {
        name: CXIdent,
        local_id: THIRLocalID,
    },
    Assign {
        target: Box<THIRExpression>,
        value: Box<THIRExpression>,
    },

    // Represents a no-op used to change the type of an expression with no added semantics
    Typechange(Box<THIRExpression>),

    // Aggregate Access
    MemberAccess {
        base: Box<THIRExpression>,
        member_index: usize,
        aggregate_type: THIRType,
    },
    ArrayAccess {
        array: Box<THIRExpression>,
        index: Box<THIRExpression>,
        element_type: THIRType,
    },
    PatternIs {
        lhs: Box<THIRExpression>,
        pattern: THIRPattern,
    },
    Unpack {
        name: CXIdent,
        local_id: THIRLocalID,
        struct_type: THIRType,
        bindings: Vec<THIRUnpackBinding>,
    },

    // Tagged Unions
    TaggedUnionTag {
        value: Box<THIRExpression>,
        sum_type: THIRType,
    },
    TaggedUnionGet {
        value: Box<THIRExpression>,
        variant_type: THIRType,
        variant_index: usize,
    },
    TaggedUnionSet {
        target: Box<THIRExpression>,
        variant_index: usize,
        inner_value: Box<THIRExpression>,
        sum_type: THIRType,
    },

    // Internal node used by generated type-constructor functions.
    TaggedUnionInitializer {
        variant_index: usize,
        value: Box<THIRExpression>,
        sum_type: THIRType,
    },
    ArrayInitializer {
        elements: Vec<THIRExpression>,
        element_type: THIRType,
    },
    StructInitializer {
        initializations: Vec<StructInitialization>,
        struct_type: THIRType,
    },

    // Control Flow
    Break,
    Continue,
    Goto {
        name: CXIdent,
    },
    Label {
        name: CXIdent,
        statement: Box<THIRExpression>,
    },
    If {
        condition: Box<THIRExpression>,
        then_branch: Box<THIRExpression>,
        else_branch: Option<Box<THIRExpression>>,
    },
    While {
        condition: Box<THIRExpression>,
        body: Box<THIRExpression>,
        pre_eval: bool,
    },
    For {
        init: Box<THIRExpression>,
        condition: Box<THIRExpression>,
        increment: Box<THIRExpression>,
        body: Box<THIRExpression>,
    },

    CSwitch {
        condition: Box<THIRExpression>,
        cases: Vec<(Box<THIRExpression>, Box<THIRExpression>)>,
        default: Option<Box<THIRExpression>>,
    },

    Match {
        condition: Box<THIRExpression>,
        subject: THIRLocalID,
        arms: Vec<(THIRPattern, Box<THIRExpression>)>,
        default: Option<Box<THIRExpression>>,
        exhaustive: bool,
    },

    Return {
        postcondition: Option<THIRPostcondition>,
        value: Option<Box<THIRExpression>>,
    },
    Yield {
        value: Option<Box<THIRExpression>>,
    },

    Assert {
        condition: Box<THIRExpression>,
        message: String,
    },

    // Sequential Statements
    Defer {
        expression: Box<THIRExpression>,
    },
    Block {
        statements: Vec<THIRExpression>,
        creates_scope: bool,
    },

    // Function Calls
    CallFunction {
        function: Box<THIRExpression>,
        arguments: Vec<THIRExpression>,
        contract: THIRFnContract,
    },

    VaStart {
        list: Box<THIRExpression>,
        last: Box<THIRExpression>,
    },
    VaEnd {
        list: Box<THIRExpression>,
    },
    VaArg {
        list: Box<THIRExpression>,
        _type: THIRType,
    },

    // Type Conversion
    TypeConversion {
        operand: Box<THIRExpression>,
        conversion: THIRCoercion,
    },

    // Lifetime Management
    LifetimeStart {
        variable: CXIdent,
        _type: THIRType,
    },
    LifetimeEnd {
        variable: CXIdent,
        _type: THIRType,
    },
    LeakLifetime {
        expression: Box<THIRExpression>,
    },

    Unsafe {
        expression: Box<THIRExpression>,
    },

    // Comptime
    Emit(Box<THIRExpression>),
}

#[derive(Clone, Debug, Readable, Writable)]
pub enum THIRIntBinOp {
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
pub enum THIRPtrDiffBinOp {
    ADD,
    SUB,
}

#[derive(Clone, Debug, Readable, Writable)]
pub enum THIRPtrBinOp {
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
}

#[derive(Clone, Debug, Readable, Writable)]
pub enum THIRFloatBinOp {
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
pub enum THIRBinOp {
    Integer {
        itype: THIRIntType,
        op: THIRIntBinOp,
    },

    Float {
        ftype: THIRFloatType,
        op: THIRFloatBinOp,
    },

    /**
     *  Any binary operation instruction of this type must have the pointer value as the lhs, and the integer value as the rhs.
     */
    PtrDiff {
        op: THIRPtrDiffBinOp,

        // Boxed for size reasons
        ptr_inner: Box<THIRType>,
    },

    Pointer {
        op: THIRPtrBinOp,
    },
}

#[derive(Clone, Debug, Readable, Writable)]
pub enum THIRUnOp {
    NEG,
    INEG,
    FNEG,
    BNOT,
    LNOT,

    PreIncrement(i8),
    PostIncrement(i8),
}

#[derive(Clone, Copy, Debug, Readable, Writable)]
pub enum THIRCoercion {
    // Any integer to any integer conversion
    Integral {
        sextend: bool,
        from_type: THIRIntType,
        to_type: THIRIntType,
    },

    // Any float to any float conversion
    FloatCast {
        to_type: THIRFloatType,
    },

    // Any integer type to a floating point number, sizes of types need not match
    IntToFloat {
        to_type: THIRFloatType,
        sextend: bool,
    },

    // Any float type to any integer type, sizes of types need not match
    FloatToInt {
        to_type: THIRIntType,
        sextend: bool,
    },

    // Pointer to any specified integer type
    PtrToInt {
        to_type: THIRIntType,
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
pub struct THIRUnpackBinding {
    pub field_name: CXIdent,
    pub field_type: THIRType,
    pub field_index: usize,

    pub binding_name: CXIdent,
    pub binding_local_id: THIRLocalID,
}

#[derive(Clone, Debug)]
pub struct StructInitialization {
    pub field_index: usize,
    pub value: THIRExpression,
}

impl THIRExpression {
    pub fn get_type(&self) -> THIRType {
        self._type.clone()
    }

    pub fn get_type_ref(&self) -> &THIRType {
        &self._type
    }

    pub fn int_literal(value: i64, itype: THIRIntType, is_signed: bool) -> Self {
        Self {
            kind: THIRExpressionKind::IntLiteral(value),
            _type: THIRType {
                kind: THIRTypeKind::Integer {
                    _type: itype,
                    signed: is_signed,
                },

                ..Default::default()
            },
            token_range: TokenRange::internal(),
        }
    }
}
