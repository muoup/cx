use std::cell::Cell;

use cx_tokens::TokenRange;
use cx_util::dense_id;
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};
use speedy::{Readable, Writable};

use crate::thir::comptime::THIRStagedExpr;
use crate::thir::data::THIRTypeID;
use crate::thir::pattern::THIRPattern;
use crate::thir::r#type::{THIRFloatType, THIRIntType, THIRType, THIRTypeKind};

thread_local! {
    static NEXT_LOCAL_ID: Cell<u64> = const { Cell::new(0) };
}

dense_id!(THIRLocalID, "local.");

impl THIRLocalID {
    pub fn fresh() -> Self {
        NEXT_LOCAL_ID.with(|next| {
            let id = next.get();
            next.set(id + 1);
            Self::new(id as usize)
        })
    }
}

#[derive(Clone, Debug, Default)]
pub struct THIRFnContract {
    safe: bool,
    precondition: Option<Box<THIRExpression>>,
    postcondition: Option<THIRPostcondition>,
}

impl THIRFnContract {
    pub fn new(
        safe: bool,
        precondition: Option<Box<THIRExpression>>,
        postcondition: Option<THIRPostcondition>,
    ) -> Self {
        Self {
            safe,
            precondition,
            postcondition,
        }
    }

    pub fn is_safe(&self) -> bool {
        self.safe
    }

    pub fn precondition(&self) -> Option<&THIRExpression> {
        self.precondition.as_deref()
    }

    pub fn postcondition(&self) -> Option<&THIRPostcondition> {
        self.postcondition.as_ref()
    }
}

#[derive(Clone, Debug)]
pub struct THIRPostcondition {
    binding: Option<CXIdent>,
    condition: Box<THIRExpression>,
}

impl THIRPostcondition {
    pub fn new(binding: Option<CXIdent>, condition: Box<THIRExpression>) -> Self {
        Self { binding, condition }
    }

    pub fn binding(&self) -> Option<&CXIdent> {
        self.binding.as_ref()
    }

    pub fn condition(&self) -> &THIRExpression {
        &self.condition
    }
}

#[derive(Clone, Debug)]
pub struct THIRExpression {
    pub kind: THIRExpressionKind,
    pub ty: THIRType,
    pub token_range: TokenRange,
}

impl Default for THIRExpression {
    fn default() -> Self {
        Self {
            kind: THIRExpressionKind::default(),
            ty: THIRType::default(),
            token_range: TokenRange::internal(),
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
        ty: THIRType,
    },
    AlignOf {
        ty: THIRType,
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
        ty: THIRType,
        initial_value: Option<Box<THIRExpression>>,
    },
    AdoptRegion {
        binding_name: CXIdent,
        local_id: THIRLocalID,
        ty: THIRType,
        initial_value: Box<THIRExpression>,
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

    AddressOf {
        operand: Box<THIRExpression>,
    },

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
        value: Box<THIRExpression>,
        bindings: Vec<THIRUnpackBinding>,
    },

    // Tagged Unions
    TaggedUnionTag {
        value: Box<THIRExpression>,
        sum_type: THIRType,
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
    },

    Return {
        postcondition: Option<THIRPostcondition>,
        value: Option<Box<THIRExpression>>,
    },
    Unreachable,
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
        kind: THIRBlockKind,
        yields: bool,
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
        ty: THIRType,
    },

    // Type Conversion
    TypeConversion {
        operand: Box<THIRExpression>,
        conversion: THIRCoercion,
    },

    // Lifetime Management
    Leak {
        expression: Box<THIRExpression>,
    },
    Unsafe {
        expression: Box<THIRExpression>,
    },

    StagedExpression(THIRStagedExpr),
    Materialize {
        expr: Box<THIRExpression>,
        with_params: Vec<THIRExpression>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum THIRBlockKind {
    Sequence,
    Statement,
    Expression,
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
        ptr_inner: THIRTypeID,
    },

    PtrDifference {
        element_ty: THIRTypeID,
    },

    Pointer {
        op: THIRPtrBinOp,
    },
}

#[derive(Clone, Debug, Readable, Writable)]
pub enum THIRUnOp {
    INEG,
    FNEG,
    BNOT,
    LNOT,

    PreIncrement(i8),
    PostIncrement(i8),
}

#[derive(Clone, Debug, Readable, Writable)]
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

    // Converts an ephemeral reference to a bounded reference
    ReferenceBounding(Vec<THIRLocalID>),

    // Conversions between types that have the same semantic meaning,
    // this is typically a no-op, but proves useful for type checking and verification
    Typechange,
    Adopt,

    // A similar no-op operation like Typechange, but represents conversions that *do* change the semantic
    // meaning of the bits, such as converting from an f32 to an i32
    //
    // Converting from a bounded / ephemeral reference to a free reference (non-safe operation) also falls under this category
    Bitcast,

    StringToArray,

    Unreachable,
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
    pub fn int_literal(value: i64, itype: THIRIntType, is_signed: bool) -> Self {
        Self {
            kind: THIRExpressionKind::IntLiteral(value),
            ty: THIRType {
                kind: THIRTypeKind::Integer {
                    ty: itype,
                    signed: is_signed,
                },

                ..Default::default()
            },
            token_range: TokenRange::internal(),
        }
    }
}
