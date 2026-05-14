use crate::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
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
    pub name: Option<CXIdent>,
    pub _type: LMIRType,
    pub abi: LMIRParameterABI,
}

#[derive(Debug, Clone)]
pub enum LMIRParameterABI {
    Normal,
    ByVal { pointee: LMIRType, alignment: u8 },
    StructReturn { pointee: LMIRType, alignment: u8 },
}

#[derive(Debug, Clone)]
pub struct LMIRFunctionSignature {
    pub return_type: LMIRType,
    pub params: Vec<LMIRParameter>,
    pub var_args: bool,
}

#[derive(Debug, Clone)]
pub enum LMIRABIArgKind {
    Direct {
        slots: Vec<LMIRABISlot>,
    },
    Indirect {
        pointee: LMIRType,
        byval: bool,
        alignment: u8,
    },
}

#[derive(Debug, Clone)]
pub struct LMIRABISlot {
    pub _type: LMIRType,
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub struct LMIRABIParameter {
    pub name: Option<CXIdent>,
    pub semantic_type: LMIRType,
    pub kind: LMIRABIArgKind,
}

#[derive(Debug, Clone)]
pub enum LMIRABIReturnKind {
    Void,
    Direct {
        slots: Vec<LMIRABISlot>,
    },
    IndirectSret {
        pointee: LMIRType,
        alignment: u8,
        returns_pointer: bool,
    },
}

#[derive(Debug, Clone)]
pub struct LMIRABISignature {
    pub return_kind: LMIRABIReturnKind,
    pub params: Vec<LMIRABIParameter>,
    pub var_args: bool,
}

impl LMIRABIArgKind {
    pub fn lowered_params(&self, name: Option<CXIdent>) -> Vec<LMIRParameter> {
        match self {
            LMIRABIArgKind::Direct { slots } => slots
                .iter()
                .enumerate()
                .map(|(i, slot)| LMIRParameter {
                    name: name.as_ref().map(|name| {
                        if slots.len() == 1 {
                            name.clone()
                        } else {
                            CXIdent::from(format!("{name}.__abi_slot_{i}"))
                        }
                    }),
                    _type: slot._type.clone(),
                    abi: LMIRParameterABI::Normal,
                })
                .collect(),
            LMIRABIArgKind::Indirect {
                pointee,
                byval,
                alignment,
            } => vec![LMIRParameter {
                name,
                _type: LMIRType::default_pointer(),
                abi: if *byval {
                    LMIRParameterABI::ByVal {
                        pointee: pointee.clone(),
                        alignment: *alignment,
                    }
                } else {
                    LMIRParameterABI::Normal
                },
            }],
        }
    }
}

impl LMIRABIReturnKind {
    pub fn lowered_type(&self) -> LMIRType {
        match self {
            LMIRABIReturnKind::Void => LMIRType::unit(),
            LMIRABIReturnKind::Direct { slots } if slots.len() == 1 => slots[0]._type.clone(),
            LMIRABIReturnKind::Direct { slots } => LMIRTypeKind::ABIAggregate {
                fields: slots.iter().map(|slot| slot._type.clone()).collect(),
            }
            .into(),
            LMIRABIReturnKind::IndirectSret {
                returns_pointer: true,
                ..
            } => LMIRType::default_pointer(),
            LMIRABIReturnKind::IndirectSret {
                returns_pointer: false,
                ..
            } => LMIRType::unit(),
        }
    }

    pub fn sret_type(&self) -> Option<LMIRType> {
        match self {
            LMIRABIReturnKind::IndirectSret { pointee, .. } => Some(pointee.clone()),
            _ => None,
        }
    }
}

impl LMIRABISignature {
    pub fn lowered_signature(&self) -> LMIRFunctionSignature {
        let mut params = self
            .params
            .iter()
            .flat_map(|param| param.kind.lowered_params(param.name.clone()))
            .collect::<Vec<_>>();

        if let LMIRABIReturnKind::IndirectSret {
            pointee,
            alignment,
            returns_pointer,
        } = &self.return_kind
        {
            params.insert(
                0,
                LMIRParameter {
                    name: Some(CXIdent::from("__internal_buffer")),
                    _type: LMIRType::default_pointer(),
                    abi: if *returns_pointer {
                        LMIRParameterABI::Normal
                    } else {
                        LMIRParameterABI::StructReturn {
                            pointee: pointee.clone(),
                            alignment: *alignment,
                        }
                    },
                },
            );
        }

        LMIRFunctionSignature {
            return_type: self.return_kind.lowered_type(),
            params,
            var_args: self.var_args,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LMIRFunctionPrototype {
    pub name: String,
    pub return_type: LMIRType,
    pub params: Vec<LMIRParameter>,
    pub var_args: bool,
    pub linkage: LinkageType,
    pub temp_buffer: Option<LMIRType>,
    pub abi_signature: LMIRABISignature,
}

impl LMIRFunctionPrototype {
    pub fn signature(&self) -> LMIRFunctionSignature {
        LMIRFunctionSignature {
            return_type: self.return_type.clone(),
            params: self.params.clone(),
            var_args: self.var_args,
        }
    }
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
        func: CXIdent,
        args: Vec<LMIRValue>,
        method_sig: LMIRFunctionSignature,
    },

    IndirectCall {
        func_ptr: LMIRValue,
        args: Vec<LMIRValue>,
        method_sig: LMIRFunctionSignature,
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
        condition: LMIRValue,
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
    FloatCast {
        from: LMIRFloatType,
    },
    IntToPtr {
        from: LMIRIntegerType,
        sextend: bool,
    },
    IntToFloat {
        from: LMIRIntegerType,
        sextend: bool,
    },
    FloatToInt {
        from: LMIRFloatType,
        sextend: bool,
    },
    PtrToInt,
    BitCast,
}
