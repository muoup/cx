use std::fmt::{Display, Formatter};
use crate::parse::pass_bytecode::typing::get_intrinsic_type;
use crate::parse::pass_molded::TypeMap;

#[derive(Debug, Clone, PartialEq)]
pub enum CXValType {
    Integer { bytes: u8, signed: bool },
    Float { bytes: u8 },
    Structured { fields: Vec<(String, CXValType)> },
    Unit,

    PointerTo(Box<CXValType>),
    Array {
        size: usize,
        _type: Box<CXValType>
    },
    Opaque {
        name: String,
        size: usize
    },

    Identifier(String)
}

impl Display for CXValType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXValType::Integer { bytes, signed } => {
                let signed_str = if *signed { "i" } else { "u" };
                let signed_bytes = *bytes * 8;
                write!(f, "{}i{}", signed_str, signed_bytes)
            },
            CXValType::Float { bytes } => {
                let float_bytes = *bytes * 8;
                write!(f, "f{}", float_bytes)
            },
            CXValType::Structured { fields } => {
                let field_strs = fields.iter()
                    .map(|(name, type_)| format!("{}: {}", name, type_))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "struct {{ {} }}", field_strs)
            },
            CXValType::Unit => write!(f, "()"),
            CXValType::PointerTo(inner) => {
                write!(f, "*{}", inner)
            },
            CXValType::Array { size, _type } => {
                write!(f, "[{}; {}]", size, _type)
            },
            CXValType::Opaque { name, size } => {
                write!(f, "OP_{}(\"{}\")", size, name)
            },
            CXValType::Identifier(name) => {
                write!(f, "{}", name)
            }
        }
    }
}

pub fn is_structure(type_map: &TypeMap, val_type: &CXValType) -> bool {
    matches!(get_intrinsic_type(type_map, val_type), Some(CXValType::Structured { .. }))
}