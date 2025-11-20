use crate::types::MIRType;
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};
use std::collections::HashMap;

mod format;
pub mod types;

pub type BCFunctionMap = HashMap<String, MIRFunctionPrototype>;

#[derive(Debug, Clone)]
pub struct MIRUnit {
    pub fn_map: BCFunctionMap,
    pub fn_defs: Vec<MIRFunction>,

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
        _type: MIRType,
        initial_value: Option<i64>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BCValue {
    NULL,
    ParameterRef(u32),
    IntImmediate { type_: MIRType, val: i64 },
    FloatImmediate { type_: MIRType, val: FloatWrapper },
    Global(ElementID),
    FunctionRef(CXIdent),
    LoadOf(MIRType, Box<BCValue>),
    BlockResult { block_id: BlockID, value_id: u32 },
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BlockID {
    Block(ElementID),
    DeferredBlock(ElementID),
}

#[derive(Debug, Clone)]
pub struct MIRParameter {
    pub name: Option<String>,
    pub _type: MIRType,
}

#[derive(Debug, Clone)]
pub struct MIRFunctionPrototype {
    pub name: String,
    pub return_type: MIRType,
    pub params: Vec<MIRParameter>,
    pub var_args: bool,
    pub linkage: LinkageType,
}

#[derive(Debug, Clone)]
pub struct MIRFunction {
    pub prototype: MIRFunctionPrototype,

    pub blocks: Vec<MIRBlock>,
    pub defer_blocks: Vec<MIRBlock>,
}

#[derive(Debug, Clone)]
pub struct MIRBlock {
    pub debug_name: String,
    pub body: Vec<MIRInstruction>,
}

#[derive(Debug, Clone)]
pub struct MIRInstruction {
    pub kind: MIRInstructionKind,
    pub value_type: MIRType,
}

#[derive(Debug, Clone)]
pub enum MIRInstructionKind {
    Allocate {
        _type: MIRType,
        alignment: u8,
    },

    StructAccess {
        struct_: BCValue,
        struct_type: MIRType,
        field_index: usize,
        field_offset: usize,
    },

    Temp {
        value: BCValue,
    },

    Store {
        memory: BCValue,
        value: BCValue,
        type_: MIRType,
    },

    ZeroMemory {
        memory: BCValue,
        _type: MIRType,
    },

    // Since a bool in Cranelift is represented as an i8, extending from an i8 to an i8
    // should be a no-op, but using a plain ZExtend attempts to convert it, thus causing
    // an error,
    BoolExtend {
        value: BCValue,
    },

    ZExtend {
        value: BCValue,
    },

    SExtend {
        value: BCValue,
    },

    Phi {
        predecessors: Vec<(BCValue, BlockID)>,
    },

    Trunc {
        value: BCValue,
    },

    IntToPtrDiff {
        value: BCValue,
        ptr_type: MIRType,
    },

    IntToPtr {
        value: BCValue,
    },

    PointerBinOp {
        op: MIRPtrBinOp,
        ptr_type: MIRType,
        left: BCValue,
        right: BCValue,
    },

    IntegerBinOp {
        op: MIRIntBinOp,
        left: BCValue,
        right: BCValue,
    },

    IntegerUnOp {
        op: MIRIntUnOp,
        value: BCValue,
    },

    FloatBinOp {
        op: MIRFloatBinOp,
        left: BCValue,
        right: BCValue,
    },

    FloatUnOp {
        op: MIRFloatUnOp,
        value: BCValue,
    },

    DirectCall {
        args: Vec<BCValue>,
        method_sig: MIRFunctionPrototype,
    },

    IndirectCall {
        func_ptr: BCValue,
        args: Vec<BCValue>,
        method_sig: MIRFunctionPrototype,
    },

    GetFunctionAddr {
        func: String,
    },

    IntToFloat {
        from: MIRType,
        value: BCValue,
    },

    FloatToInt {
        from: MIRType,
        value: BCValue,
    },

    PtrToInt {
        value: BCValue,
    },

    FloatCast {
        value: BCValue,
    },

    Branch {
        condition: BCValue,
        true_block: BlockID,
        false_block: BlockID,
    },

    GotoDefer,

    Jump {
        target: BlockID,
    },

    JumpTable {
        value: BCValue,
        targets: Vec<(u64, BlockID)>,
        default: BlockID,
    },

    Return {
        value: Option<BCValue>,
    },

    BitCast {
        value: BCValue,
    },
    
    // During debug builds, we can insert assertions to validate assumptions
    // During release builds, these can either be no-ops or compiler hints
    CompilerAssertion {
        condition: BCValue,
        message: BCValue,
    },

    NOP,
}

impl MIRInstructionKind {
    pub fn is_block_terminating(&self) -> bool {
        matches!(self, MIRInstructionKind::JumpTable { .. }
            | MIRInstructionKind::Branch { .. }
            | MIRInstructionKind::Jump { .. }
            | MIRInstructionKind::Return { .. })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRPtrBinOp {
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
pub enum MIRIntBinOp {
    ADD,
    SUB,
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
pub enum MIRIntUnOp {
    BNOT,
    LNOT,
    NEG,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRFloatBinOp {
    ADD,
    SUB,
    FMUL,
    FDIV,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRFloatUnOp {
    NEG,
}
