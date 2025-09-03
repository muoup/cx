use std::fmt::{Display, Formatter};
use crate::cx_types::{CXFunctionPrototype, CXParameter, CXType, CXTypeKind};

impl Display for CXFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}({}) -> {}",
               self.name.as_string(),
               self.params.iter().map(|p| format!("{p}")).collect::<Vec<_>>().join(", "),
               self.return_type
        )
    }
}

impl Display for CXType {
    
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for CXTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXTypeKind::Integer { bytes, signed } => {
                let signed_str = if *signed { "i" } else { "u" };
                let signed_bytes = *bytes * 8;
                write!(f, "{signed_str}{signed_bytes}")
            },
            CXTypeKind::Float { bytes } => {
                let float_bytes = *bytes * 8;
                write!(f, "f{float_bytes}")
            },
            CXTypeKind::Bool => write!(f, "bool"),
            CXTypeKind::Structured { fields, name } => {
                let field_strs = fields.iter()
                    .map(|(name, type_)| format!("{name}: {type_}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let name_str = if let Some(name) = name {
                    format!("{name} ")
                } else {
                    "".to_string()
                };

                write!(f, "struct {name_str} {{ {field_strs} }}")
            },
            CXTypeKind::Union { fields, name } => {
                let field_strs = fields.iter()
                    .map(|(name, type_)| format!("{name}: {type_}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let name_str = if let Some(name) = name {
                    format!("{name} ")
                } else {
                    "".to_string()
                };

                write!(f, "union {name_str} {{ {field_strs} }}")
            },
            CXTypeKind::Unit => write!(f, "()"),
            CXTypeKind::PointerTo { inner_type: inner, weak: explicitly_weak, nullable, sizeless_array } => {
                write!(f, "{inner} ")?;

                if *explicitly_weak {
                    write!(f, "weak")?;
                }

                if *sizeless_array {
                    write!(f, "[]")?;
                } else {
                    write!(f, "*")?;
                }

                if !*nullable {
                    write!(f, " (nonnull)")
                } else {
                    Ok(())
                }
            },
            CXTypeKind::StrongPointer { inner_type: inner, is_array } => {
                write!(f, "{inner} strong")?;
                if *is_array {
                    write!(f, "[]")
                } else {
                    write!(f, "*")
                }
            },
            CXTypeKind::Array { size, inner_type: _type } => {
                write!(f, "[{size}; {_type}]")
            },
            CXTypeKind::VariableLengthArray { _type, .. } => {
                write!(f, "[{_type}; variable]")
            },
            CXTypeKind::Opaque { name, size } => {
                write!(f, "OPAQUE_{size}(\"{name}\")")
            },
            CXTypeKind::Function { prototype } => {
                write!(f, "{prototype}")
            },
            CXTypeKind::MemoryReference(inner) => {
                write!(f, "&{inner}")
            },
        }
    }
}


impl Display for CXParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}: {}", name, self._type)
        } else {
            write!(f, "{}", self._type)
        }
    }
}
