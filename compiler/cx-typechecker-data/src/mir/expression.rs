use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::mir::types::{CXFloatType, CXFunctionPrototype, CXIntegerType, CXType, CXTypeKind};

pub type BCRegister = CXIdent;

#[derive(Clone, Debug, Default)]
pub enum MIRValue {
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
        prototype: CXFunctionPrototype,
        implicit_variables: Vec<MIRValue>
    },
    GlobalValue {
        name: CXIdent,
        _type: CXType,
    },
    Parameter {
        index: usize,
        _type: CXType,
    },
    Register {
        register: BCRegister,
        _type: CXType,
    },

    #[default]
    NULL,
}

#[derive(Clone, Debug)]
pub enum MIRInstruction {
    Alias {
        result: BCRegister,
        value: MIRValue,
    },

    CreateStackRegion {
        result: BCRegister,
        _type: CXType,
    },
    
    CreateRegionCopy {
        result: BCRegister,
        source: BCRegister,
        _type: CXType,
    },

    LoadGlobal {
        result: BCRegister,
        name: CXIdent,
    },

    MemoryRead {
        result: BCRegister,
        source: MIRValue,
        _type: CXType,
    },

    MemoryWrite {
        target: MIRValue,
        value: MIRValue,
    },

    StructGet {
        result: BCRegister,
        source: MIRValue,
        field_index: usize,
        struct_type: CXType,
    },
    
    TaggedUnionGet {
        result: BCRegister,
        source: MIRValue,
        variant_type: CXType,
    },
    
    TaggedUnionIs {
        result: BCRegister,
        source: MIRValue,
        tag_id: usize
    },

    ArrayGet {
        result: BCRegister,
        source: MIRValue,
        index: MIRValue,
        element_type: CXType,
    },

    CallFunction {
        result: Option<BCRegister>,
        function: MIRValue,
        arguments: Vec<MIRValue>,
    },
    
    ConstructSumType {
        result: BCRegister,
        variant_index: usize,
        value: MIRValue,
        sum_type: CXType
    },

    Loop {
        condition_precheck: bool,
        condition: MIRValue,
        body: CXIdent,
        merge: CXIdent,
    },

    LoopContinue {
        loop_id: CXIdent,
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
        result: BCRegister,
        lhs: MIRValue,
        rhs: MIRValue,
        op: MIRBinOp,
    },

    UnOp {
        result: BCRegister,
        operand: MIRValue,
        op: MIRUnOp,
    },

    Coercion {
        result: BCRegister,
        operand: MIRValue,
        cast_type: MIRCoercion,
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
        target: BCRegister,
    },
}

#[derive(Clone, Debug)]
pub enum MIRBinOp {
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    IMUL,
    IDIV,
    IMOD,
    
    FADD,
    FSUB,
    FMUL,
    FDIV,

    AND,
    OR,
    XOR,
    SHL,
    SHR,

    EQ,
    NEQ,

    LT,
    LE,
    GT,
    GE,
    ILT,
    ILE,
    IGT,
    IGE,
    FLT,
    FLE,
    FGT,
    FGE,
}

#[derive(Clone, Debug)]
pub enum MIRUnOp {
    NEG,
    INEG,
    FNEG,
    BNOT,
    LNOT,
}

#[derive(Clone, Debug)]
pub enum MIRCoercion {
    Integral {
        sextend: bool,
        to_type: CXIntegerType,
    },
    FPIntegral {
        to_type: CXFloatType,
    },
    PtrToInt,
    IntToPtr,
    IntToFloat,
    IntToBool,
    BoolToInt,
    FloatToInt,
    ReinterpretBits,
}

impl MIRValue {
    pub fn get_type(&self) -> CXType {
        match self {
            MIRValue::IntLiteral { _type, signed, .. } => CXType::from(CXTypeKind::Integer {
                _type: _type.clone(),
                signed: *signed,
            }),
            MIRValue::FloatLiteral { _type, .. } => CXType::from(CXTypeKind::Float {
                _type: _type.clone(),
            }),
            MIRValue::FunctionReference { prototype, .. } => CXType::from(CXTypeKind::Function {
                prototype: Box::new(prototype.clone()),
            })
            .pointer_to(),
            MIRValue::Parameter { _type, .. } |
            MIRValue::GlobalValue { _type, .. } |
            MIRValue::Register { _type, .. } => _type.clone(),
            MIRValue::NULL => CXType::unit(),
        }
    }
}