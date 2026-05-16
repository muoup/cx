use crate::mir::data::{MIRType, MIRTypeContext, MIRTypeKind};
use crate::mir::r#type::MIRField;

pub fn base_mangle_standard(name: &str) -> String {
    name.to_string()
}

pub fn base_mangle_member(
    definitions: &MIRTypeContext,
    name: &str,
    member_type: &MIRType,
) -> String {
    format!("_M{}_{}", definitions.mangle(member_type), name)
}

pub fn base_mangle_static_member(
    definitions: &MIRTypeContext,
    name: &str,
    member_type: &MIRType,
) -> String {
    format!("_S{}_{}", definitions.mangle(member_type), name)
}

pub(crate) fn type_mangle(definitions: &MIRTypeContext, ty: &MIRType) -> String {
    let mut mangled = String::new();

    match &ty.kind {
        MIRTypeKind::PointerTo { inner_type } => {
            mangled.push('P');
            let inner_type = definitions
                .get(*inner_type)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0));
            mangled.push_str(&type_mangle(definitions, inner_type));
        }
        MIRTypeKind::MemoryReference {
            inner_type,
            bitfield,
        } => {
            mangled.push('R');
            if let Some(bitfield) = bitfield {
                mangled.push('b');
                mangled.push_str(&bitfield.bit_offset.to_string());
                mangled.push('_');
                mangled.push_str(&bitfield.bit_width.to_string());
                mangled.push('_');
            }
            let inner_type = definitions
                .get(*inner_type)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0));
            mangled.push_str(&type_mangle(definitions, inner_type));
        }
        MIRTypeKind::Opaque { size } => {
            mangled.push('O');
            mangled.push_str(&size.to_string());
        }
        MIRTypeKind::Array {
            length: size,
            inner_type,
        } => {
            mangled.push('A');
            mangled.push_str(&size.to_string());
            mangled.push('_');
            let inner_type = definitions
                .get(*inner_type)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0));
            mangled.push_str(&type_mangle(definitions, inner_type));
        }
        MIRTypeKind::Function { signature } => {
            mangled.push('F');
            mangled.push_str(&type_mangle(definitions, &signature.return_type));
            for param in &signature.params {
                mangled.push_str(&type_mangle(definitions, &param._type));
            }
            mangled.push(if signature.var_args { 'V' } else { 'v' });
        }
        MIRTypeKind::Structured { fields } => {
            mangled.push('S');
            push_identifier(&mut mangled, ty);
            push_move_attributes(&mut mangled, ty);
            if ty.get_name().is_none() {
                push_aggregate_fields(&mut mangled, definitions, fields);
            }
        }
        MIRTypeKind::Union { variants } => {
            mangled.push('U');
            push_identifier(&mut mangled, ty);
            if ty.get_name().is_none() {
                push_aggregate_fields(&mut mangled, definitions, variants);
            }
        }
        MIRTypeKind::TaggedUnion { variants } => {
            mangled.push('T');
            push_identifier(&mut mangled, ty);
            push_move_attributes(&mut mangled, ty);
            if ty.get_name().is_none() {
                push_aggregate_fields(&mut mangled, definitions, variants);
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
        MIRTypeKind::Undefined => {
            mangled.push('X');
            if let Some(name) = ty.get_name() {
                mangled.push_str(name.as_str().len().to_string().as_str());
                mangled.push('_');
                mangled.push_str(name.as_str());
            }
        }
        MIRTypeKind::Unit => {
            mangled.push('v');
        }
    }

    mangled
}

fn push_identifier(mangled: &mut String, ty: &MIRType) {
    if let Some(name) = ty.get_name() {
        mangled.push('n');
        mangled.push_str(name.as_str().len().to_string().as_str());
        mangled.push('_');
        mangled.push_str(name.as_str());
    }
}

fn push_move_attributes(mangled: &mut String, ty: &MIRType) {
    mangled.push(if ty.move_attributes.nocopy { 'C' } else { 'c' });
    mangled.push(if ty.move_attributes.nodrop { 'D' } else { 'd' });
}

fn push_aggregate_fields(mangled: &mut String, definitions: &MIRTypeContext, fields: &[MIRField]) {
    mangled.push('f');
    mangled.push_str(&fields.len().to_string());
    mangled.push('_');
    for field in fields {
        let field_id = field.type_id();
        if matches!(field, MIRField::Bitfield { .. }) {
            mangled.push('b');
        }
        let field_type = definitions
            .get(field_id)
            .unwrap_or_else(|| panic!("Unknown type id {}", field_id.0));
        mangled.push_str(&type_mangle(definitions, field_type));
    }
}
