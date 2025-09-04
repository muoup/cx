use std::fmt::{Display, Formatter};
use crate::parse::identifier::CXIdent;
use crate::preparse::CXNaiveFnIdent;
use crate::preparse::naive_types::{CXNaivePrototype, CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind};

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
            CXNaiveTypeKind::StrongPointer { inner, is_array } => write!(f, "{inner} strong* [is_array={is_array}]"),
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
            CXNaiveTypeKind::FunctionPointer { prototype } => write!(f, "FunctionPointer({prototype})"),
        }
    }
}

impl Display for CXNaivePrototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self.params.iter()
            .map(|param| format!("{}: {}", param.name.as_ref().unwrap_or(&CXIdent::from("_")), param._type))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{} {}({})", self.return_type, self.name, params_str)
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

impl Display for CXNaiveFnIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXNaiveFnIdent::Standard(name) => write!(f, "{}", name),
            CXNaiveFnIdent::MemberFunction { _type, function_name } => {
                let Some(name) = _type.get_name() else {
                    unreachable!("Member function type must have a name");
                };

                write!(f, "_{}_{}", name, function_name)
            }
            CXNaiveFnIdent::Destructor(name) => {
                write!(f, "~{}", name)
            }
        }
    }
}
