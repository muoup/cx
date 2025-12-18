use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::mir::types::{CXFloatType, MIRFunctionPrototype, CXIntegerType, MIRType, MIRTypeKind};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MIRRegister {
    pub name: CXIdent,
}

#[derive(Clone, Debug, Default)]
pub enum MIRValue {
    BoolLiteral {
        value: bool,
    },
    IntLiteral {
        value: i64,
        signed: bool,
        _type: CXIntegerType,
    },
    FloatLiteral {
        value: FloatWrapper,
        _type: CXFloatType,
    },
    FunctionReference {
        prototype: MIRFunctionPrototype,
        implicit_variables: Vec<MIRValue>,
    },
    GlobalValue {
        name: CXIdent,
        _type: MIRType,
    },
    Parameter {
        name: CXIdent,
        _type: MIRType,
    },
    Register {
        register: MIRRegister,
        _type: MIRType,
    },

    #[default]
    NULL,
}

#[derive(Clone, Debug)]
pub enum MIRInstruction {
    Alias {
        result: MIRRegister,
        value: MIRValue,
    },

    CreateStackRegion {
        result: MIRRegister,
        _type: MIRType,
    },

    CopyRegionInto {
        destination: MIRValue,
        source: MIRValue,
        _type: MIRType,
    },

    ConstructTaggedUnionInto {
        variant_index: usize,
        memory: MIRValue,
        value: MIRValue,
        sum_type: MIRType,
    },

    MemoryRead {
        result: MIRRegister,
        source: MIRValue,
        _type: MIRType,
    },

    MemoryWrite {
        target: MIRValue,
        value: MIRValue,
    },

    StructGet {
        result: MIRRegister,
        source: MIRValue,
        field_index: usize,
        field_offset: usize,
        struct_type: MIRType,
    },
    
    TaggedUnionTag {
        result: MIRRegister,
        source: MIRValue,
        sum_type: MIRType,
    },

    TaggedUnionGet {
        result: MIRRegister,
        source: MIRValue,
        variant_type: MIRType,
    },
    
    ArrayGet {
        result: MIRRegister,
        source: MIRValue,
        index: MIRValue,
        array_type: MIRType,
        element_type: MIRType,
    },

    CallFunction {
        result: Option<MIRRegister>,
        function: MIRValue,
        arguments: Vec<MIRValue>,
    },

    LoopPreHeader {
        loop_id: CXIdent,
        condition_precheck: bool,
        condition_block: CXIdent,
        body_block: CXIdent,
    },

    LoopConditionBranch {
        loop_id: CXIdent,
        condition: MIRValue,
        body_block: CXIdent,
        exit_block: CXIdent,
    },

    LoopContinue {
        loop_id: CXIdent,
        condition_block: CXIdent,
    },

    Branch {
        condition: MIRValue,
        true_block: CXIdent,
        false_block: CXIdent,
    },

    Jump {
        target: CXIdent,
    },

    JumpTable {
        condition: MIRValue,
        targets: Vec<(u64, CXIdent)>,
        default: CXIdent,
    },

    Return {
        value: Option<MIRValue>,
    },

    BinOp {
        result: MIRRegister,
        lhs: MIRValue,
        rhs: MIRValue,
        op: MIRBinOp,
    },

    UnOp {
        result: MIRRegister,
        operand: MIRValue,
        op: MIRUnOp,
    },

    Coercion {
        result: MIRRegister,
        operand: MIRValue,
        cast_type: MIRCoercion,
    },
    
    Phi {
        result: MIRRegister,
        predecessors: Vec<(MIRValue, CXIdent)>,
    },

    LifetimeStart {
        name: String,
        region: MIRRegister,
        _type: MIRType,
    },
    
    LifetimeEnd {
        name: String,
        region: MIRRegister,
        _type: MIRType,
    },
    
    LeakLifetime {
        region: MIRRegister,
        _type: MIRType,
    },
    
    // ---- Verification Nodes ----
    
    Assert {
        value: MIRValue,
        message: String,
    },

    Assume {
        value: MIRValue,
    },

    Havoc {
        target: MIRRegister,
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
        sextend: bool
    },

    // Any integer type to a boolean (i1)
    IntToBool,
    
    // Conversions between equally sized types that do not change the bit representation,
    // in assembly, this is typically a no-op, but proves useful for type checking and verification
    ReinterpretBits,
}

impl MIRInstruction {
    pub fn is_block_terminator(&self) -> bool {
        matches!(
            self,
            MIRInstruction::Return { .. }
                | MIRInstruction::Jump { .. }
                | MIRInstruction::Branch { .. }
                | MIRInstruction::LoopPreHeader { .. }
                | MIRInstruction::LoopConditionBranch { .. }
                | MIRInstruction::LoopContinue { .. }
                | MIRInstruction::JumpTable { .. }
        )
    }
}

impl MIRValue {
    pub fn get_type(&self) -> MIRType {
        match self {
            MIRValue::IntLiteral { _type, signed, .. } => MIRType::from(MIRTypeKind::Integer {
                _type: *_type,
                signed: *signed,
            }),
            MIRValue::FloatLiteral { _type, .. } => MIRType::from(MIRTypeKind::Float {
                _type: *_type,
            }),
            MIRValue::FunctionReference { prototype, .. } => MIRType::from(MIRTypeKind::Function {
                prototype: Box::new(prototype.clone()),
            })
            .pointer_to(),
            MIRValue::BoolLiteral { .. } => MIRType::from(MIRTypeKind::Bool),
            MIRValue::Parameter { _type, .. }
            | MIRValue::GlobalValue { _type, .. }
            | MIRValue::Register { _type, .. } => _type.clone(),
            MIRValue::NULL => MIRType::unit(),
        }
    }
}

impl MIRRegister {
    pub fn new<T: Into<CXIdent>>(name: T) -> Self {
        MIRRegister { name: name.into() }
    }
}

impl From<MIRRegister> for CXIdent {
    fn from(val: MIRRegister) -> Self {
        val.name
    }
}
