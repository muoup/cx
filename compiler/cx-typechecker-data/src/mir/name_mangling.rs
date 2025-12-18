use crate::mir::types::{MIRType, MIRTypeKind};

pub fn base_mangle_standard(name: &str) -> String {
    format!("{}", name)
}

pub fn base_mangle_member(name: &str, member_type: &MIRType) -> String {
    format!("_M{}_{}", member_type.mangle(), name)
}

pub fn base_mangle_destructor(_type: &MIRType) -> String {
    format!("_D_{}", _type.mangle())
}

pub fn base_mangle_deconstructor(_type: &MIRType) -> String {
    format!("_DC_{}", _type.mangle())
}

pub(crate) fn type_mangle(ty: &MIRType) -> String {
    let mut mangled = String::new();

    match &ty.kind {
        MIRTypeKind::PointerTo { inner_type, .. } => {
            mangled.push('P');
            mangled.push_str(&type_mangle(inner_type));
        }
        MIRTypeKind::MemoryReference(inner_type) => {
            mangled.push('R');
            mangled.push_str(&type_mangle(inner_type));
        }
        MIRTypeKind::Opaque { size, .. } => {
            mangled.push('O');
            mangled.push_str(&size.to_string());
        }
        MIRTypeKind::Array { size, inner_type } => {
            mangled.push('A');
            mangled.push_str(&size.to_string());
            mangled.push('_');
            mangled.push_str(&type_mangle(inner_type));
        }
        MIRTypeKind::Function { prototype } => {
            mangled.push('F');
            mangled.push_str(&type_mangle(&prototype.return_type));
            for param in &prototype.params {
                mangled.push_str(&type_mangle(&param._type));
            }
            mangled.push(prototype.var_args as u8 as char);
        }
        MIRTypeKind::Structured { name, fields, .. } => {
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
        MIRTypeKind::Union { variants, .. } => {
            mangled.push('U');
            mangled.push_str(&variants.len().to_string());
            mangled.push('_');
            for variant in variants {
                mangled.push_str(&type_mangle(&variant.1));
            }
        }
        MIRTypeKind::TaggedUnion { variants, .. } => {
            mangled.push('T');
            mangled.push_str(&variants.len().to_string());
            mangled.push('_');
            for variant in variants {
                mangled.push_str(&type_mangle(&variant.1));
            }
        }
        MIRTypeKind::Integer { _type, signed } => {
            mangled.push('I');
            mangled.push_str(&_type.bytes().to_string());
            mangled.push(if *signed { 's' } else { 'u' });
        }
        MIRTypeKind::Float { _type } => {
            mangled.push('F');
            mangled.push_str(&_type.bytes().to_string());
        }
        MIRTypeKind::Bool => {
            mangled.push('B');
        }
        MIRTypeKind::Unit => {
            mangled.push('v');
        }
    }

    mangled
}
