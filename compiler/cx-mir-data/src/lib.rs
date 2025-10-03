use crate::types::MIRType;
use cx_util::identifier::CXIdent;
use std::collections::HashMap;

mod format;
pub mod types;

pub type BCFunctionMap = HashMap<String, MIRFunctionPrototype>;

#[derive(Debug, Clone)]
pub struct ProgramMIR {
    pub fn_map: BCFunctionMap,
    pub fn_defs: Vec<MIRFunction>,

    pub global_vars: Vec<MIRGlobalValue>,
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
pub struct MIRGlobalValue {
    pub name: CXIdent,
    pub _type: MIRGlobalType,
    pub linkage: LinkageType,
}

#[derive(Debug, Clone)]
pub enum MIRGlobalType {
    StringLiteral(String),
    Variable {
        _type: MIRType,
        initial_value: Option<i64>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum MIRValue {
    NULL,
    ParameterRef(u32),
    IntImmediate { type_: MIRType, val: i64 },
    FloatImmediate { type_: MIRType, val: i64 },
    Global(ElementID),
    FunctionRef(CXIdent),
    LoadOf(MIRType, Box<MIRValue>),
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

    pub blocks: Vec<FunctionBlock>,
    pub defer_blocks: Vec<FunctionBlock>,
}

#[derive(Debug, Clone)]
pub struct FunctionBlock {
    pub debug_name: String,
    pub body: Vec<BlockInstruction>,
}

#[derive(Debug, Clone)]
pub struct BlockInstruction {
    pub instruction: VirtualInstruction,
    pub value_type: MIRType,
}

#[derive(Debug, Clone)]
pub enum VirtualInstruction {
    Allocate {
        _type: MIRType,
        alignment: u8,
    },

    StructAccess {
        struct_: MIRValue,
        struct_type: MIRType,
        field_index: usize,
        field_offset: usize,
    },

    Temp {
        value: MIRValue,
    },

    Store {
        memory: MIRValue,
        value: MIRValue,
        type_: MIRType,
    },

    ZeroMemory {
        memory: MIRValue,
        _type: MIRType,
    },

    // Since a bool in Cranelift is represented as an i8, extending from an i8 to an i8
    // should be a no-op, but using a plain ZExtend attempts to convert it, thus causing
    // an error,
    BoolExtend {
        value: MIRValue,
    },

    ZExtend {
        value: MIRValue,
    },

    SExtend {
        value: MIRValue,
    },

    Phi {
        predecessors: Vec<(MIRValue, BlockID)>,
    },

    Trunc {
        value: MIRValue,
    },

    IntToPtrDiff {
        value: MIRValue,
        ptr_type: MIRType,
    },

    IntToPtr {
        value: MIRValue,
    },

    PointerBinOp {
        op: BCPtrBinOp,
        ptr_type: MIRType,
        left: MIRValue,
        right: MIRValue,
    },

    IntegerBinOp {
        op: MIRIntBinOp,
        left: MIRValue,
        right: MIRValue,
    },

    IntegerUnOp {
        op: BCIntUnOp,
        value: MIRValue,
    },

    FloatBinOp {
        op: BCFloatBinOp,
        left: MIRValue,
        right: MIRValue,
    },

    FloatUnOp {
        op: BCFloatUnOp,
        value: MIRValue,
    },

    DirectCall {
        args: Vec<MIRValue>,
        method_sig: MIRFunctionPrototype,
    },

    IndirectCall {
        func_ptr: MIRValue,
        args: Vec<MIRValue>,
        method_sig: MIRFunctionPrototype,
    },

    GetFunctionAddr {
        func: String,
    },

    IntToFloat {
        from: MIRType,
        value: MIRValue,
    },

    FloatToInt {
        from: MIRType,
        value: MIRValue,
    },

    PtrToInt {
        value: MIRValue,
    },

    FloatCast {
        value: MIRValue,
    },

    Branch {
        condition: MIRValue,
        true_block: BlockID,
        false_block: BlockID,
    },

    GotoDefer,

    Jump {
        target: BlockID,
    },

    JumpTable {
        value: MIRValue,
        targets: Vec<(u64, BlockID)>,
        default: BlockID,
    },

    Return {
        value: Option<MIRValue>,
    },

    BitCast {
        value: MIRValue,
    },

    NOP,
}

impl VirtualInstruction {
    pub fn is_block_terminating(&self) -> bool {
        match self {
            VirtualInstruction::JumpTable { .. }
            | VirtualInstruction::Branch { .. }
            | VirtualInstruction::Jump { .. }
            | VirtualInstruction::Return { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub enum BCIntUnOp {
    BNOT,
    LNOT,
    NEG,
}

#[derive(Debug, Clone, Copy)]
pub enum BCFloatBinOp {
    ADD,
    SUB,
    FMUL,
    FDIV,
}

#[derive(Debug, Clone, Copy)]
pub enum BCFloatUnOp {
    NEG,
}
