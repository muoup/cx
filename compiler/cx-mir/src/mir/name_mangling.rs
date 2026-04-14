use crate::mir::data::{MIRAggregateContents, MIRType, MIRTypeKind};

pub fn base_mangle_standard(name: &str) -> String {
    name.to_string()
}

pub fn base_mangle_member(name: &str, member_type: &MIRType) -> String {
    format!("_M{}_{}", member_type.mangle(), name)
}

pub fn base_mangle_static_member(name: &str, member_type: &MIRType) -> String {
    format!("_S{}_{}", member_type.mangle(), name)
}

pub(crate) fn type_mangle(ty: &MIRType) -> String {
    let mut mangled = String::new();

    match &ty.kind {
        MIRTypeKind::PointerTo { inner_type, .. } => {
            mangled.push('P');
            mangled.push_str(&type_mangle(inner_type));
        }
        MIRTypeKind::MemoryReference { inner_type } => {
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
            mangled.push(if prototype.var_args { 'V' } else { 'v' });
        }
        MIRTypeKind::Structured {
            fields,
            ..
        } => {
            mangled.push('S');

            if let Some(n) = name {
                mangled.push('n');
                mangled.push_str(n.as_str().len().to_string().as_str());
                mangled.push('_');
                mangled.push_str(n.as_str());
            }

            mangled.push(if attributes.nocopy { 'C' } else { 'c' });
            mangled.push(if attributes.nodrop { 'D' } else { 'd' });

            match fields {
                MIRAggregateContents::Named(_) => {
                    mangled.push('n');
                }
                MIRAggregateContents::Anonymous(fields) => {
                    mangled.push('f');
                    mangled.push_str(&fields.len().to_string());
                    mangled.push('_');
                    for field in fields {
                        mangled.push_str(&type_mangle(&field.1));
                    }
                }
            }
        }
        MIRTypeKind::Union { name, variants } => {
            mangled.push('U');

            if let Some(n) = name {
                mangled.push('n');
                mangled.push_str(n.as_str().len().to_string().as_str());
                mangled.push('_');
                mangled.push_str(n.as_str());
            }

            match variants {
                MIRAggregateContents::Named(_) => {
                    mangled.push('n');
                }
                MIRAggregateContents::Anonymous(variants) => {
                    mangled.push_str(&variants.len().to_string());
                    mangled.push('_');
                    for variant in variants {
                        mangled.push_str(&type_mangle(&variant.1));
                    }
                }
            }
        }
        MIRTypeKind::TaggedUnion {
            name,
            attributes,
            variants,
            ..
        } => {
            mangled.push('T');

            mangled.push('n');
            mangled.push_str(name.as_str().len().to_string().as_str());
            mangled.push('_');
            mangled.push_str(name.as_str());

            mangled.push(if attributes.nocopy { 'C' } else { 'c' });
            mangled.push(if attributes.nodrop { 'D' } else { 'd' });

            match variants {
                MIRAggregateContents::Named(_) => {
                    mangled.push('n');
                }
                MIRAggregateContents::Anonymous(variants) => {
                    mangled.push_str(&variants.len().to_string());
                    mangled.push('_');
                    for variant in variants {
                        mangled.push_str(&type_mangle(&variant.1));
                    }
                }
            }
        }
        MIRTypeKind::Integer { _type, signed } => {
            mangled.push_str(format!("{}", _type).as_str());
            mangled.push(if *signed { 's' } else { 'u' });
        }
        MIRTypeKind::Float { _type } => {
            mangled.push_str(format!("{}", _type).as_str());
        }
        MIRTypeKind::Str => {
            mangled.push_str("_str");
        }
        MIRTypeKind::Undefined { name } => {
            mangled.push('X');
            mangled.push_str(name.as_str().len().to_string().as_str());
            mangled.push('_');
            mangled.push_str(name.as_str());
        }
        MIRTypeKind::Unit => {
            mangled.push('v');
        }
    }

    mangled
}
