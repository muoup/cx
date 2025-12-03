use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::mir::types::{CXFloatType, CXFunctionPrototype, CXIntegerType, CXType, CXTypeKind};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MIRRegister {
    pub name: CXIdent,
}

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
        implicit_variables: Vec<MIRValue>,
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
        register: MIRRegister,
        _type: CXType,
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

    CreateEmptyStackRegion {
        result: MIRRegister,
        _type: CXType,
    },

    CopyStackRegionInto {
        result: MIRRegister,
        source: MIRRegister,
        _type: CXType,
    },

    ConstructTaggedUnionInto {
        variant_index: usize,
        memory: MIRValue,
        value: MIRValue,
        sum_type: CXType,
    },

    MemoryRead {
        result: MIRRegister,
        source: MIRValue,
        _type: CXType,
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
        struct_type: CXType,
    },

    TaggedUnionGet {
        result: MIRRegister,
        source: MIRValue,
        variant_type: CXType,
    },

    TaggedUnionIs {
        result: MIRRegister,
        source: MIRValue,
        tag_id: usize,
    },

    ArrayGet {
        result: MIRRegister,
        source: MIRValue,
        index: MIRValue,
        element_type: CXType,
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
        body_block: CXIdent
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
    ADD, SUB, MUL, DIV, MOD, 
    IMUL, IDIV, IMOD,
    
    EQ, NE, LT, LE, GT, GE,
    ILT, ILE, IGT, IGE,
}

#[derive(Clone, Debug)]
pub enum MIRPtrDiffBinOp {
    ADD,
    SUB
}

#[derive(Clone, Debug)]
pub enum MIRBoolBinOp {
    LAND,
    LOR,
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
        op: MIRFloatBinOp
    },
    
    /**
     *  Any binary operation instruction of this type must have the pointer value as the lhs, and the integer value as the rhs.
     */
    PtrDiff {
        op: MIRPtrDiffBinOp,
        
        // Boxed for size reasons
        ptr_inner: Box<CXType>
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
            MIRValue::Parameter { _type, .. }
            | MIRValue::GlobalValue { _type, .. }
            | MIRValue::Register { _type, .. } => _type.clone(),
            MIRValue::NULL => CXType::unit(),
        }
    }
}

impl MIRRegister {
    pub fn new<T: Into<CXIdent>>(name: T) -> Self {
        MIRRegister { name: name.into() }
    }
}

impl Into<CXIdent> for MIRRegister {
    fn into(self) -> CXIdent {
        self.name
    }
}
