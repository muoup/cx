use crate::types::BCType;
use std::collections::HashMap;
use cx_util::identifier::CXIdent;

pub mod types;
mod format;

pub type BCFunctionMap = HashMap<String, BCFunctionPrototype>;

#[derive(Debug, Clone)]
pub struct ProgramBytecode {
    pub fn_map: BCFunctionMap,
    pub fn_defs: Vec<BytecodeFunction>,
    
    pub global_vars: Vec<BCGlobalValue>
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum LinkageType {
    ODR,
    Static,
    Public,
    Private,
    External
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
        initial_value: Option<i64>
    },
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum ValueID {
    NULL,
    Global(ElementID),
    Block(BlockID, ElementID)
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BlockID {
    Block(ElementID),
    DeferredBlock(ElementID)
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
    pub linkage: LinkageType,
}

#[derive(Debug, Clone)]
pub struct BytecodeFunction {
    pub prototype: BCFunctionPrototype,
    
    pub blocks: Vec<FunctionBlock>,
    pub defer_blocks: Vec<FunctionBlock>,
}

#[derive(Debug, Clone)]
pub struct FunctionBlock {
    pub debug_name: String,
    pub body: Vec<BlockInstruction>
}

#[derive(Debug, Clone)]
pub struct BlockInstruction {
    pub instruction: VirtualInstruction,
    pub value: VirtualValue
}

#[derive(Debug, Clone)]
pub enum VirtualInstruction {
    FunctionParameter {
        param_index: u32
    },

    Allocate {
        _type: BCType,
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
    
    ZeroMemory {
        memory: ValueID,
        _type: BCType
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
    
    IntToPtr {
        value: ValueID
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
        name: String,
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

impl BCGlobalValue {
    pub fn as_virtual_value(&self) -> VirtualValue {
        match &self._type {
            BCGlobalType::StringLiteral(s) => VirtualValue {
                type_: BCType::default_pointer()
            },
            BCGlobalType::Variable { _type, .. } => VirtualValue {
                type_: _type.clone()
            },
        }
    }
}