use std::fmt::Display;

use crate::{
    function_map::{CXFunctionIdentifier, CXFunctionKind},
    mir::types::{CXType, CXTypeKind},
};

pub(crate) fn type_mangle(ty: &CXType) -> String {
    let mut mangled = String::new();

    match &ty.kind {
        CXTypeKind::PointerTo { inner_type, .. } => {
            mangled.push('P');
            mangled.push_str(&type_mangle(inner_type));
        }
        CXTypeKind::StrongPointer {
            inner_type,
            is_array,
        } => {
            mangled.push('S');
            if *is_array {
                mangled.push('A');
            } else {
                mangled.push('P');
            }
            mangled.push_str(&type_mangle(inner_type));
        }
        CXTypeKind::MemoryReference(inner_type) => {
            mangled.push('R');
            mangled.push_str(&type_mangle(inner_type));
        }
        CXTypeKind::Opaque { size, .. } => {
            mangled.push('O');
            mangled.push_str(&size.to_string());
        }
        CXTypeKind::Array { size, inner_type } => {
            mangled.push('A');
            mangled.push_str(&size.to_string());
            mangled.push('_');
            mangled.push_str(&type_mangle(inner_type));
        }
        CXTypeKind::Function { prototype } => {
            mangled.push('F');
            mangled.push_str(&type_mangle(&prototype.return_type));
            for param in &prototype.params {
                mangled.push_str(&type_mangle(&param._type));
            }
            mangled.push(prototype.var_args as u8 as char);
        }
        CXTypeKind::Structured { name, fields, .. } => {
            mangled.push('S');

            if let Some(n) = name {
                mangled.push('n');
                mangled.push_str(n.as_str().len().to_string().as_str());
                mangled.push('_');
                mangled.push_str(n.as_str());
            }

            mangled.push('f');
            mangled.push_str(&fields.len().to_string());
            mangled.push('_');
            for field in fields {
                mangled.push_str(&type_mangle(&field.1));
            }
        }
        CXTypeKind::Union { variants, .. } => {
            mangled.push('U');
            mangled.push_str(&variants.len().to_string());
            mangled.push('_');
            for variant in variants {
                mangled.push_str(&type_mangle(&variant.1));
            }
        }
        CXTypeKind::TaggedUnion { variants, .. } => {
            mangled.push('T');
            mangled.push_str(&variants.len().to_string());
            mangled.push('_');
            for variant in variants {
                mangled.push_str(&type_mangle(&variant.1));
            }
        }
        CXTypeKind::Integer { _type, signed } => {
            mangled.push('I');
            mangled.push_str(&_type.bytes().to_string());
            mangled.push(if *signed { 's' } else { 'u' });
        }
        CXTypeKind::Float { _type } => {
            mangled.push('F');
            mangled.push_str(&_type.bytes().to_string());
        }
        CXTypeKind::Bool => {
            mangled.push('B');
        }
        CXTypeKind::Unit => {
            mangled.push('v');
        }
    }

    mangled
}

impl Display for CXFunctionIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind = match &self {
            CXFunctionIdentifier::Standard { kind } => kind,
            CXFunctionIdentifier::Templated { kind, .. } => kind,
        };

        match &kind {
            CXFunctionKind::Standard { name } => write!(f, "{}", name.as_string()),
            CXFunctionKind::Member { base_type, name } => {
                write!(f, "{}::{}", base_type.as_string(), name.as_string())
            }
            CXFunctionKind::Destructor { base_type } => {
                write!(f, "{}::~{}", base_type.as_string(), base_type.as_string())
            }
            CXFunctionKind::Deconstructor { base_type } => {
                write!(f, "{}::__deconstruct", base_type.as_string())
            }
        }
    }
}
