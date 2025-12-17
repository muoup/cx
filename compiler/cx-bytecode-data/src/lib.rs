use crate::types::{BCFloatType, BCIntegerType, BCType};
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};
use std::collections::HashMap;

mod format;
pub mod types;

pub type BCFunctionMap = HashMap<String, BCFunctionPrototype>;

#[derive(Debug, Clone)]
pub struct BCUnit {
    pub fn_map: BCFunctionMap,
    pub fn_defs: Vec<BCFunction>,

    pub global_vars: Vec<BCGlobalValue>,
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
pub struct BCGlobalValue {
    pub name: CXIdent,
    pub _type: BCGlobalType,
    pub linkage: LinkageType,
}

#[derive(Debug, Clone)]
pub enum BCGlobalType {
    StringLiteral(String),
    Variable {
        _type: BCType,
        initial_value: Option<i64>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BCValue {
    NULL,
    Register {
        register: BCRegister,
        _type: BCType,
    },
    ParameterRef(u32),
    IntImmediate {
        _type: BCIntegerType,
        val: i64,
    },
    FloatImmediate {
        _type: BCFloatType,
        val: FloatWrapper,
    },
    BoolImmediate(bool),
    Global(ElementID),
    FunctionRef(CXIdent),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BCRegister {
    pub name: CXIdent,
}
pub type BCBlockID = CXIdent;

impl BCRegister {
    pub fn new<T: Into<CXIdent>>(name: T) -> Self {
        BCRegister { name: name.into() }
    }
}

impl From<BCRegister> for CXIdent {
    fn from(val: BCRegister) -> Self {
        val.name
    }
}

#[derive(Debug, Clone)]
pub struct BCParameter {
    pub name: Option<String>,
    pub _type: BCType,
}

#[derive(Debug, Clone)]
pub struct BCFunctionPrototype {
    pub name: String,
    pub return_type: BCType,
    pub params: Vec<BCParameter>,
    pub var_args: bool,
    pub linkage: LinkageType,
    pub temp_buffer: Option<BCType>
}

#[derive(Debug, Clone)]
pub struct BCFunction {
    pub prototype: BCFunctionPrototype,
    pub blocks: Vec<BCBasicBlock>,
}

#[derive(Debug, Clone)]
pub struct BCBasicBlock {
    pub id: BCBlockID,
    pub body: Vec<BCInstruction>,
}

#[derive(Debug, Clone)]
pub struct BCInstruction {
    pub kind: BCInstructionKind,
    pub value_type: BCType,
    pub result: Option<BCRegister>,
}

#[derive(Debug, Clone)]
pub enum BCInstructionKind {
    Allocate {
        _type: BCType,
        alignment: u8,
    },

    StructAccess {
        struct_: BCValue,
        struct_type: BCType,
        field_index: usize,
        field_offset: usize,
    },

    Alias {
        value: BCValue,
    },

    Store {
        memory: BCValue,
        value: BCValue,
        _type: BCType,
    },
    
    Memcpy {
        dest: BCValue,
        src: BCValue,
        size: BCValue,
        alignment: u8,
    },

    Load {
        memory: BCValue,
        _type: BCType,
    },

    ZeroMemory {
        memory: BCValue,
        _type: BCType,
    },

    Coercion {
        value: BCValue,
        coercion_type: BCCoercionType,
    },

    Phi {
        predecessors: Vec<(BCValue, BCBlockID)>,
    },

    PointerBinOp {
        op: BCPtrBinOp,
        ptr_type: BCType,
        type_padded_size: u64,
        left: BCValue,
        right: BCValue,
    },

    IntegerBinOp {
        op: BCIntBinOp,
        left: BCValue,
        right: BCValue,
    },
    
    BooleanBinOp {
        op: BCBoolBinOp,
        left: BCValue,
        right: BCValue,
    },

    IntegerUnOp {
        op: BCIntUnOp,
        value: BCValue,
    },

    FloatBinOp {
        op: BCFloatBinOp,
        left: BCValue,
        right: BCValue,
    },

    FloatUnOp {
        op: BCFloatUnOp,
        value: BCValue,
    },
    
    BooleanUnOp {
        op: BCBoolUnOp,
        value: BCValue,
    },

    DirectCall {
        args: Vec<BCValue>,
        method_sig: BCFunctionPrototype,
    },

    IndirectCall {
        func_ptr: BCValue,
        args: Vec<BCValue>,
        method_sig: BCFunctionPrototype,
    },

    GetFunctionAddr {
        func: String,
    },

    Branch {
        condition: BCValue,
        true_block: BCBlockID,
        false_block: BCBlockID,
    },

    Jump {
        target: BCBlockID,
    },

    JumpTable {
        value: BCValue,
        targets: Vec<(u64, BCBlockID)>,
        default: BCBlockID,
    },

    Return {
        value: Option<BCValue>,
    },

    CompilerAssumption {
        condition: BCValue
    },
}

impl BCInstructionKind {
    pub fn is_block_terminating(&self) -> bool {
        matches!(
            self,
            BCInstructionKind::JumpTable { .. }
                | BCInstructionKind::Branch { .. }
                | BCInstructionKind::Jump { .. }
                | BCInstructionKind::Return { .. }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BCPtrBinOp {
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
pub enum BCIntBinOp {
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
pub enum BCBoolBinOp {
    LAND,
    LOR,
    
    EQ,
    NE
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BCIntUnOp {
    BNOT,
    LNOT,
    NEG,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BCFloatBinOp {
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
pub enum BCBoolUnOp {
    LNOT,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BCFloatUnOp {
    NEG,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BCCoercionType {
    BoolExtend,
    ZExtend,
    SExtend,
    Trunc,
    FloatCast { from: BCFloatType },
    IntToPtr { from: BCIntegerType, sextend: bool },
    IntToFloat { from: BCIntegerType, sextend: bool },
    FloatToInt { from: BCFloatType, sextend: bool },
    PtrToInt,
    BitCast
}
