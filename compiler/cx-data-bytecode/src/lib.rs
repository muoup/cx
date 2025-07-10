use crate::types::BCType;
use std::collections::HashMap;

pub mod mangling;
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
    pub block_id: BlockID,
    pub value_id: ElementID
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockID {
    pub in_deferral: bool,
    pub id: ElementID
}

impl ValueID {
    pub const NULL: Self = ValueID {
        block_id: BlockID {
            in_deferral: true,
            id: u32::MAX
        },
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
    pub _type: BCType
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
    
    pub blocks: Vec<FunctionBlock>,
    pub defer_blocks: Vec<FunctionBlock>,
    
    pub static_linkage: bool,
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

pub const POINTER_TAG : usize = 0xF000_0000_0000_0000;

#[derive(Debug)]
pub enum VirtualInstruction {
    FunctionParameter {
        param_index: u32
    },
    
    VariableAllocate {
        size: ValueID,
        alignment: u8,
    },

    Allocate {
        size: usize,
        alignment: u8,
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

    // Since a bool in Cranelift is represented as an i8, extending from an i8 to an i8
    // should be a no-op, but using a plain ZExtend attempts to convert it, thus causing
    // an error,
    BoolExtend {
        value: ValueID,
    },
    
    ZExtend {
        value: ValueID,
    },

    SExtend {
        value: ValueID,
    },
    
    Phi {
        predecessors: Vec<(ValueID, BlockID)>,
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
        func: ValueID
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
        true_block: BlockID,
        false_block: BlockID
    },

    GotoDefer,

    Jump {
        target: BlockID
    },
    
    JumpTable {
        value: ValueID,
        targets: Vec<(u64, BlockID)>,
        default: BlockID
    },

    Return {
        value: Option<ValueID>
    },

    BitCast {
        value: ValueID
    },
    
    AddPointerTag {
        value: ValueID
    },
    
    ClearPointerTag {
        value: ValueID
    },
    
    HasPointerTag {
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
    LT, GT, LE, GE,
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