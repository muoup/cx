use std::fmt::{Display, Formatter};
use crate::parse::pass_ast::identifier::CXIdent;
use crate::parse::pass_bytecode::typing::get_intrinsic_type;
use crate::parse::pass_ast::TypeMap;

#[derive(Debug, Clone, PartialEq)]
pub enum CXValType {
    Integer { bytes: u8, signed: bool },
    Float { bytes: u8 },
    Structured { fields: Vec<(String, CXValType)> },
    Unit,

    PointerTo(Box<CXValType>),
    MemoryReference(Box<CXValType>),
    Array {
        size: usize,
        _type: Box<CXValType>
    },
    Opaque {
        name: String,
        size: usize
    },
    Function {
        return_type: Box<CXValType>,
        args: Vec<CXValType>
    },

    Identifier(CXIdent)
}

pub fn is_structure(type_map: &TypeMap, val_type: &CXValType) -> bool {
    matches!(get_intrinsic_type(type_map, val_type), Some(CXValType::Structured { .. }))
}