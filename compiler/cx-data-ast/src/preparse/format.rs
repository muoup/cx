use std::fmt::Display;
use crate::preparse::pp_type::{CXNaivePrototype, CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind};

impl Display for CXNaiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for CXNaiveTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CXNaiveTypeKind::Identifier { name, .. } => write!(f, "{name}"),
            CXNaiveTypeKind::TemplatedIdentifier { name, input } => write!(f, "{name}{input}"),
            CXNaiveTypeKind::ExplicitSizedArray(inner, size) => write!(f, "[{inner}; {size}]"),
            CXNaiveTypeKind::ImplicitSizedArray(inner) => write!(f, "[{inner}]"),
            CXNaiveTypeKind::PointerTo { inner_type, weak } => write!(f, "{}{}", if *weak { "weak " } else { "" }, inner_type),
            CXNaiveTypeKind::StrongPointer { inner, is_array } => write!(f, "StrongPointer<{inner}; {is_array}>"),
            CXNaiveTypeKind::Structured { name, fields } => {
                let fields_str = fields.iter()
                    .map(|(name, ty)| format!("{name}: {ty}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "Structured<{}> {{ {} }}", name.as_ref().map(|n| n.as_str()).unwrap_or(""), fields_str)
            },
            CXNaiveTypeKind::Union { name, fields } => {
                let fields_str = fields.iter()
                    .map(|(name, ty)| format!("{name}: {ty}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "Union<{}> {{ {} }}", name.as_ref().map(|n| n.as_str()).unwrap_or(""), fields_str)
            },
            CXNaiveTypeKind::FunctionPointer { prototype } => write!(f, "FunctionPointer<{prototype}>"),
        }
    }
}

impl Display for CXNaivePrototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self.params.iter()
            .map(|param| format!("{:?}: {}", param.name, param._type))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}({}) -> {}", self.return_type, params_str, self.name)
    }
}

impl Display for CXNaiveTemplateInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self.params.iter()
            .map(|param| param.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "<{params_str}>")
    }
}