use crate::types::{LMIRFloatType, LMIRIntegerType, LMIRType};
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};
use std::collections::HashMap;

mod format;
pub mod types;

pub type LMIRFunctionMap = HashMap<String, LMIRFunctionPrototype>;

#[derive(Debug, Clone)]
pub struct LMIRUnit {
    pub fn_map: LMIRFunctionMap,
    pub fn_defs: Vec<LMIRFunction>,

    pub global_vars: Vec<LMIRGlobalValue>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum LinkageType {
    ODR,
    Static,
    Standard,
    External,
}

pub type ElementID = u32;

#[derive(Debug, Clone)]
pub struct LMIRGlobalValue {
    pub name: CXIdent,
    pub _type: LMIRGlobalType,
    pub linkage: LinkageType,
}

#[derive(Debug, Clone)]
pub enum LMIRGlobalType {
    StringLiteral(String),
    Variable {
        _type: LMIRType,
        initial_value: Option<i64>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum LMIRValue {
    NULL,
    Register {
        register: LMIRRegister,
        _type: LMIRType,
    },
    ParameterRef(u32),
    IntImmediate {
        _type: LMIRIntegerType,
        val: i64,
    },
    FloatImmediate {
        _type: LMIRFloatType,
        val: FloatWrapper,
    },
    Global(ElementID),
    FunctionRef(CXIdent),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LMIRRegister {
    pub name: CXIdent,
}
pub type LMIRBlockID = CXIdent;

impl LMIRRegister {
    pub fn new<T: Into<CXIdent>>(name: T) -> Self {
        LMIRRegister { name: name.into() }
    }
}

impl From<LMIRRegister> for CXIdent {
    fn from(val: LMIRRegister) -> Self {
        val.name
    }
}

#[derive(Debug, Clone)]
pub struct LMIRParameter {
    pub name: Option<String>,
    pub _type: LMIRType,
}

#[derive(Debug, Clone)]
pub struct LMIRFunctionPrototype {
    pub name: String,
    pub return_type: LMIRType,
    pub params: Vec<LMIRParameter>,
    pub var_args: bool,
    pub linkage: LinkageType,
    pub temp_buffer: Option<LMIRType>
}

#[derive(Debug, Clone)]
pub struct LMIRFunction {
    pub prototype: LMIRFunctionPrototype,
    pub blocks: Vec<LMIRBasicBlock>,
}

#[derive(Debug, Clone)]
pub struct LMIRBasicBlock {
    pub id: LMIRBlockID,
    pub debug_name: Option<String>,
    pub body: Vec<LMIRInstruction>,
}

#[derive(Debug, Clone)]
pub struct LMIRInstruction {
    pub kind: LMIRInstructionKind,
    pub value_type: LMIRType,
    pub result: Option<LMIRRegister>,
}

#[derive(Debug, Clone)]
pub enum LMIRInstructionKind {
    Allocate {
        _type: LMIRType,
        alignment: u8,
    },

    StructAccess {
        struct_: LMIRValue,
        struct_type: LMIRType,
        field_index: usize,
        field_offset: usize,
    },

    Alias {
        value: LMIRValue,
    },

    Store {
        memory: LMIRValue,
        value: LMIRValue,
        _type: LMIRType,
    },
    
    Memcpy {
        dest: LMIRValue,
        src: LMIRValue,
        size: LMIRValue,
        alignment: u8,
    },

    Load {
        memory: LMIRValue,
        _type: LMIRType,
    },

    ZeroMemory {
        memory: LMIRValue,
        _type: LMIRType,
    },

    Coercion {
        value: LMIRValue,
        coercion_type: LMIRCoercionType,
    },

    Phi {
        predecessors: Vec<(LMIRValue, LMIRBlockID)>,
    },

    PointerBinOp {
        op: LMIRPtrBinOp,
        ptr_type: LMIRType,
        type_padded_size: u64,
        left: LMIRValue,
        right: LMIRValue,
    },

    IntegerBinOp {
        op: LMIRIntBinOp,
        left: LMIRValue,
        right: LMIRValue,
    },

    IntegerUnOp {
        op: LMIRIntUnOp,
        value: LMIRValue,
    },

    FloatBinOp {
        op: LMIRFloatBinOp,
        left: LMIRValue,
        right: LMIRValue,
    },

    FloatUnOp {
        op: LMIRFloatUnOp,
        value: LMIRValue,
    },

    DirectCall {
        args: Vec<LMIRValue>,
        method_sig: LMIRFunctionPrototype,
    },

    IndirectCall {
        func_ptr: LMIRValue,
        args: Vec<LMIRValue>,
        method_sig: LMIRFunctionPrototype,
    },

    GetFunctionAddr {
        func: String,
    },

    Branch {
        condition: LMIRValue,
        true_block: LMIRBlockID,
        false_block: LMIRBlockID,
    },

    Jump {
        target: LMIRBlockID,
    },

    JumpTable {
        value: LMIRValue,
        targets: Vec<(u64, LMIRBlockID)>,
        default: LMIRBlockID,
    },

    Return {
        value: Option<LMIRValue>,
    },

    CompilerAssumption {
        condition: LMIRValue
    },
}

impl LMIRInstructionKind {
    pub fn is_block_terminating(&self) -> bool {
        matches!(
            self,
            LMIRInstructionKind::JumpTable { .. }
                | LMIRInstructionKind::Branch { .. }
                | LMIRInstructionKind::Jump { .. }
                | LMIRInstructionKind::Return { .. }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LMIRPtrBinOp {
    ADD,
    SUB,

    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LMIRIntBinOp {
    ADD,
    SUB,
    IMUL,
    MUL,
    IDIV,
    UDIV,
    IREM,
    UREM,

    ASHR,
    LSHR,
    SHL,

    BAND,
    BOR,
    BXOR,
    LAND,
    LOR,

    EQ,
    NE,
    ILT,
    IGT,
    ULT,
    UGT,
    ILE,
    IGE,
    ULE,
    UGE,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LMIRIntUnOp {
    BNOT,
    LNOT,
    NEG,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LMIRFloatBinOp {
    ADD,
    SUB,
    FMUL,
    FDIV,

    EQ,
    NEQ,
    FLT,
    FLE,
    FGT,
    FGE,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LMIRFloatUnOp {
    NEG,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LMIRCoercionType {
    ZExtend,
    SExtend,
    Trunc,
    FloatCast { from: LMIRFloatType },
    IntToPtr { from: LMIRIntegerType, sextend: bool },
    IntToFloat { from: LMIRIntegerType, sextend: bool },
    FloatToInt { from: LMIRFloatType, sextend: bool },
    PtrToInt,
    BitCast
}
