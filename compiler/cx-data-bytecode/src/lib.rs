use crate::types::BCType;
use std::collections::HashMap;

pub mod node_type_map;
pub mod types;
mod format;

pub type BCTypeMap = HashMap<String, BCType>;
pub type BCFunctionMap = HashMap<String, BCFunctionPrototype>;

#[derive(Debug)]
pub struct ProgramBytecode {
    pub fn_map: BCFunctionMap,
    pub type_map: BCTypeMap,

    pub global_strs: Vec<String>,
    pub fn_defs: Vec<BytecodeFunction>,
}

pub type ElementID = u32;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ValueID {
    pub block_id: ElementID,
    pub value_id: ElementID
}

impl ValueID {
    pub const NULL: Self = ValueID {
        block_id: u32::MAX,
        value_id: u32::MAX
    };
}

#[derive(Debug, Clone)]
pub struct VirtualValue {
    pub type_: BCType
}

#[derive(Debug, Clone)]
pub struct BCParameter {
    pub name: Option<String>,
    pub type_: BCType
}

#[derive(Debug, Clone)]
pub struct BCFunctionPrototype {
    pub name: String,
    pub return_type: BCType,
    pub params: Vec<BCParameter>,
    pub var_args: bool,
}

#[derive(Debug)]
pub struct BytecodeFunction {
    pub prototype: BCFunctionPrototype,
    pub blocks: Vec<FunctionBlock>
}

#[derive(Debug)]
pub struct FunctionBlock {
    pub debug_name: String,
    pub body: Vec<BlockInstruction>
}

#[derive(Debug)]
pub struct BlockInstruction {
    pub instruction: VirtualInstruction,
    pub value: VirtualValue
}

#[derive(Debug)]
pub enum VirtualInstruction {
    FunctionParameter {
        param_index: u32
    },

    Allocate {
        size: usize
    },

    Load {
        value: ValueID,
    },

    Immediate {
        value: i32
    },
    
    FloatImmediate {
        value: f64
    },

    StructAccess {
        struct_: ValueID,
        struct_type: BCType,
        field_index: usize,
        field_offset: usize
    },

    Store {
        memory: ValueID,
        value: ValueID,
        type_: BCType
    },

    ZExtend {
        value: ValueID,
    },

    SExtend {
        value: ValueID,
    },

    Trunc {
        value: ValueID
    },
    
    IntToPtrDiff {
        value: ValueID,
        ptr_type: BCType
    },
    
    PointerBinOp {
        op: BCPtrBinOp,
        ptr_type: BCType,
        left: ValueID,
        right: ValueID,
    },

    IntegerBinOp {
        op: BCIntBinOp,
        left: ValueID,
        right: ValueID
    },

    IntegerUnOp {
        op: BCIntUnOp,
        value: ValueID
    },

    FloatBinOp {
        op: BCFloatBinOp,
        left: ValueID,
        right: ValueID
    },

    FloatUnOp {
        op: BCFloatUnOp,
        value: ValueID
    },

    StringLiteral {
        str_id: ElementID
    },

    DirectCall {
        func: ValueID,
        args: Vec<ValueID>,
        method_sig: BCFunctionPrototype
    },

    IndirectCall {
        func_ptr: ValueID,
        args: Vec<ValueID>,
        method_sig: BCFunctionPrototype
    },

    FunctionReference {
        name: String
    },

    GetFunctionAddr {
        func_name: ValueID
    },

    IntToFloat {
        from: BCType,
        value: ValueID
    },

    FloatToInt {
        from: BCType,
        value: ValueID
    },
    
    PtrToInt {
        value: ValueID
    },

    FloatCast {
        value: ValueID
    },

    Branch {
        condition: ValueID,
        true_block: ElementID,
        false_block: ElementID
    },

    Jump {
        target: ElementID
    },
    
    JumpTable {
        value: ValueID,
        targets: Vec<(u64, ElementID)>,
        default: ElementID
    },

    Return {
        value: Option<ValueID>
    },

    BitCast {
        value: ValueID
    },

    NOP
}

impl VirtualInstruction {
    pub fn is_block_terminating(&self) -> bool {
        match self {
            VirtualInstruction::JumpTable { .. } |
            VirtualInstruction::Branch { .. } |
            VirtualInstruction::Jump   { .. } |
            VirtualInstruction::Return { .. } => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BCPtrBinOp {
    ADD, SUB,
    
    EQ, NE,
    LT, GT, LE, GE
}

#[derive(Debug, Clone, Copy)]
pub enum BCIntBinOp {
    ADD, SUB,
    MUL, IDIV, UDIV, IREM, UREM,

    ASHR, LSHR, SHL,

    BAND, BOR, BXOR,
    LAND, LOR,
    
    EQ, NE,
    ILT, IGT, ULT, UGT,
    ILE, IGE, ULE, UGE
}

#[derive(Debug, Clone, Copy)]
pub enum BCIntUnOp {
    BNOT, LNOT, NEG
}

#[derive(Debug, Clone, Copy)]
pub enum BCFloatBinOp {
    ADD, SUB,
    FMUL, FDIV
}

#[derive(Debug, Clone, Copy)]
pub enum BCFloatUnOp {
    NEG
}