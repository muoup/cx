use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Integer { bytes: u8, signed: bool },
    Float { bytes: u8 },
    Structured { fields: Vec<(String, ValueType)> },
    Unit,

    PointerTo(Box<ValueType>),
    Array {
        size: usize,
        _type: Box<ValueType>
    },
    Opaque {
        name: String,
        size: usize
    },

    Identifier(String)
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Integer { bytes, signed } => {
                let signed_str = if *signed { "i" } else { "u" };
                let signed_bytes = *bytes * 8;
                write!(f, "{}i{}", signed_str, signed_bytes)
            },
            ValueType::Float { bytes } => {
                let float_bytes = *bytes * 8;
                write!(f, "f{}", float_bytes)
            },
            ValueType::Structured { fields } => {
                let field_strs = fields.iter()
                    .map(|(name, type_)| format!("{}: {}", name, type_))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "struct {{ {} }}", field_strs)
            },
            ValueType::Unit => write!(f, "()"),
            ValueType::PointerTo(inner) => {
                write!(f, "*{}", inner)
            },
            ValueType::Array { size, _type } => {
                write!(f, "[{}; {}]", size, _type)
            },
            ValueType::Opaque { name, size } => {
                write!(f, "OP_{}(\"{}\")", size, name)
            },
            ValueType::Identifier(name) => {
                write!(f, "{}", name)
            }
        }
    }
}