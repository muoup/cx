use crate::preparse::naive_types::{
    CXNaivePrototype, CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind,
};
use crate::preparse::NaiveFnIdent;
use cx_util::identifier::CXIdent;
use std::fmt::{Display, Formatter};

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
            CXNaiveTypeKind::PointerTo { inner_type, weak } => {
                write!(f, "{}{}", if *weak { "weak " } else { "" }, inner_type)
            }
            CXNaiveTypeKind::StrongPointer { inner, is_array } => {
                write!(f, "{inner} strong* [is_array={is_array}]")
            }
            CXNaiveTypeKind::Structured { name, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|(name, ty)| format!("{name}: {ty}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(
                    f,
                    "struct {} {{ {} }}",
                    name.as_ref().map(|n| n.as_str()).unwrap_or(""),
                    fields_str
                )
            }
            CXNaiveTypeKind::Union { name, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|(name, ty)| format!("{name}: {ty}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(
                    f,
                    "union {} {{ {} }}",
                    name.as_ref().map(|n| n.as_str()).unwrap_or(""),
                    fields_str
                )
            }
            CXNaiveTypeKind::TaggedUnion { name, variants } => {
                let variants_str = variants
                    .iter()
                    .map(|(name, ty)| format!("{name}: {ty}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "union class {name} {{ {variants_str} }}")
            }
            CXNaiveTypeKind::FunctionPointer { prototype } => {
                write!(f, "FunctionPointer({prototype})")
            }
        }
    }
}

impl Display for CXNaivePrototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self
            .params
            .iter()
            .map(|param| {
                format!(
                    "{}: {}",
                    param.name.as_ref().unwrap_or(&CXIdent::from("_")),
                    param._type
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{} {}({})", self.return_type, self.name, params_str)
    }
}

impl Display for CXNaiveTemplateInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self
            .params
            .iter()
            .map(|param| param.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "<{params_str}>")
    }
}

impl Display for NaiveFnIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NaiveFnIdent::Standard(name) => write!(f, "{name}"),
            NaiveFnIdent::MemberFunction {
                _type,
                function_name,
            } => {
                write!(f, "_{_type}_{function_name}")
            }
            NaiveFnIdent::Destructor(name) => {
                write!(f, "~{name}")
            }
        }
    }
}
