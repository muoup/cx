use cx_mir_data::{MIRFloatBinOp, MIRFloatUnOp, MIRIntBinOp, MIRIntUnOp, MIRPtrBinOp};
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::bc_type::{BCFunctionPrototype, BCType, FloatType, IntegerType};

/**
 *  Bytecode instructions are a backend-agnostic SSA IR to make it easier for CX to support multiple
 *  backends. On top of Cranelift and LLVM, CX may also support a custom VM in the future for faster
 *  prototyping and development, as well as potentially an in-house codegen backend for smaller and
 *  less dependant builds of the compiler.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BCInstruction {
    Allocate {
        result: BCAddress,
        type_: BCType,
        alignment: u8,
    },

    Store {
        destination: BCAddress,
        value: BCValue,
        store_type: BCType,
    },

    Load {
        result: BCAddress,
        source: BCValue,
        load_type: BCType,
    },

    Return {
        value: Option<BCValue>,
    },

    IntBinOp {
        result: BCAddress,
        left: BCValue,
        right: BCValue,
        op: MIRIntBinOp,
    },

    FloatBinOp {
        result: BCAddress,
        left: BCValue,
        right: BCValue,
        op: MIRFloatBinOp,
    },
    
    PointerBinOp {
        result: BCAddress,
        ptr_type: BCType,
        left: BCValue,
        right: BCValue,
        op: MIRPtrBinOp,
    },

    IntUnOp {
        result: BCAddress,
        value: BCValue,
        op: MIRIntUnOp,
    },

    FloatUnOp {
        result: BCAddress,
        value: BCValue,
        op: MIRFloatUnOp,
    },

    CallDirect {
        result: Option<BCAddress>,
        function: BCFunctionPrototype,
        arguments: Vec<BCValue>,
    },

    CallIndirect {
        result: Option<BCAddress>,
        prototype: BCFunctionPrototype,
        function_pointer: BCValue,
        arguments: Vec<BCValue>,
    },

    Branch {
        condition: BCValue,
        true_target: CXIdent,
        false_target: CXIdent,
    },

    Jump {
        target: CXIdent,
    },
    
    GetElementPtr {
        result: BCAddress,
        base: BCValue,
        index: BCValue,
        offset: BCValue,
        structure_type: BCType
    },
    
    GetFunctionPtr {
        result: BCAddress,
        function: CXIdent,
    },
    
    ValueCoercion {
        result: BCAddress,
        value: BCValue,
        coercion: BCValueCoercion,
    },
    
    Memset {
        result: BCAddress,
        value: u8,
        _type: BCType
    },
    
    Phi {
        result: BCAddress,
        predecessors: Vec<(CXIdent, BCValue)>,
    },
    
    JumpTable {
        index: BCValue,
        targets: Vec<CXIdent>,
        default_target: CXIdent,
    },
    
    Alias {
        result: BCAddress,
        source: BCAddress,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BCAddress {
    Local(CXIdent),
    Global(CXIdent),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BCValue {
    Integer {
        value: i64,
        type_: IntegerType,
    },
    Float {
        value: FloatWrapper,
        type_: FloatType,
    },
    Address(BCAddress),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BCIntegerBinOp {
    ADD,
    SUB,
    MUL,
    DIV,
    IMUL,
    IDIV,

    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
    ULT,
    UGT,
    ULE,
    UGE,

    BAND,
    BOR,
    BXOR,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BCFloatBinOp {
    ADD,
    SUB,
    MUL,
    DIV,

    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BCIntegerUnOp {
    NEG,
    BNOT,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BCFloatUnOp {
    NEG,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BCValueCoercion {
    BoolCoercion { to: IntegerType },
    SExtend { to: IntegerType },
    ZExtend { to: IntegerType },
    Truncate { to: IntegerType },
    FloatCoercion { to: FloatType },
    IntToFloat { from: IntegerType, to: FloatType },
    FloatToInt { from: FloatType, to: IntegerType },
    IntToPtrDiff { ptr_type: BCType },
    IntToPtr,
    PtrToInt,
    FunctionToPtr,
}

impl BCValue {
    pub fn get_address(&self) -> &BCAddress {
        match self {
            BCValue::Address(address) => address,
            
            _ => panic!("Expected Value or GlobalValue, got: {self:?}"),
        }
    }
}