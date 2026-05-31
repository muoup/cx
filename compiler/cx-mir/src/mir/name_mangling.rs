use crate::mir::data::{MIRType, MIRTypeKind};
use crate::mir::r#type::MIRField;
use crate::registry::MIRSymbolRegistry;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

pub fn base_mangle_standard(name: &str) -> String {
    if !name.contains("::") {
        return name.to_string();
    }

    format!("_N{}", name.replace("::", "_"))
}

pub fn mangle_namespace_symbol(name: &QualifiedName) -> String {
    let mut mangled = String::from("_N");

    for segment in name.namespace.segments() {
        push_symbol_segment(&mut mangled, segment);
    }

    mangled.push('_');
    push_symbol_segment(&mut mangled, &name.name);
    mangled
}

pub fn base_mangle_member(
    definitions: &MIRSymbolRegistry,
    name: &str,
    member_type: &MIRType,
) -> String {
    format!("_M{}_{}", type_mangle(definitions, member_type), name)
}

pub fn base_mangle_static_member(
    definitions: &MIRSymbolRegistry,
    name: &str,
    member_type: &MIRType,
) -> String {
    format!("_S{}_{}", type_mangle(definitions, member_type), name)
}

pub(crate) fn type_mangle(registry: &MIRSymbolRegistry, ty: &MIRType) -> String {
    let mut mangled = String::new();

    match &ty.kind {
        MIRTypeKind::PointerTo { inner_type } => {
            mangled.push('P');
            let inner_type = registry
                .resolve_type_id(inner_type)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0));
            mangled.push_str(&type_mangle(registry, inner_type));
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
            let inner_type = registry
                .resolve_type_id(inner_type)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0));
            mangled.push_str(&type_mangle(registry, inner_type));
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
            let inner_type = registry
                .resolve_type_id(inner_type)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0));
            mangled.push_str(&type_mangle(registry, inner_type));
        }
        MIRTypeKind::Function { signature } => {
            mangled.push('F');
            mangled.push_str(&type_mangle(registry, &signature.return_type));
            for param in &signature.params {
                mangled.push_str(&type_mangle(registry, &param._type));
            }
            mangled.push(if signature.var_args { 'V' } else { 'v' });
        }
        MIRTypeKind::Structured { fields } => {
            mangled.push('S');
            push_identifier(&mut mangled, registry, ty);
            push_move_attributes(&mut mangled, ty);
            if ty.get_name().is_none() {
                push_aggregate_fields(&mut mangled, registry, fields);
            }
        }
        MIRTypeKind::Union { variants } => {
            mangled.push('U');
            push_identifier(&mut mangled, registry, ty);
            if ty.get_name().is_none() {
                push_aggregate_fields(&mut mangled, registry, variants);
            }
        }
        MIRTypeKind::TaggedUnion { variants } => {
            mangled.push('T');
            push_identifier(&mut mangled, registry, ty);
            push_move_attributes(&mut mangled, ty);
            if ty.get_name().is_none() {
                push_aggregate_fields(&mut mangled, registry, variants);
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
                push_symbol_segment(&mut mangled, name);
            }
        }
        MIRTypeKind::Unit => {
            mangled.push('v');
        }
    }

    mangled
}

fn push_identifier(mangled: &mut String, definitions: &MIRSymbolRegistry, ty: &MIRType) {
    if let Some(name) = ty.get_name() {
        mangled.push('n');
        if let Some(strong_name) = ty.strong_identifier() {
            mangled.push_str(&mangle_namespace_symbol(strong_name));
        } else {
            push_symbol_segment(mangled, name);
        }

        if let Some(template_info) = ty.get_template_data() {
            mangled.push('T');
            for arg in &template_info.template_input.args {
                mangled.push_str(&type_mangle(definitions, arg));
            }
            mangled.push('E');
        }
    }
}

fn push_symbol_segment(mangled: &mut String, name: &CXIdent) {
    mangled.push_str(name.as_str().len().to_string().as_str());
    mangled.push('_');
    mangled.push_str(name.as_str());
}

fn push_move_attributes(mangled: &mut String, ty: &MIRType) {
    mangled.push(if ty.move_attributes.nocopy { 'C' } else { 'c' });
    mangled.push(if ty.move_attributes.nodrop { 'D' } else { 'd' });
}

fn push_aggregate_fields(mangled: &mut String, definitions: &MIRSymbolRegistry, fields: &[MIRField]) {
    mangled.push('f');
    mangled.push_str(&fields.len().to_string());
    mangled.push('_');
    for field in fields {
        let field_id = field.type_id();
        if matches!(field, MIRField::Bitfield { .. }) {
            mangled.push('b');
        }
        let field_type = definitions
            .resolve_type_id(&field_id)
            .unwrap_or_else(|| panic!("Unknown type id {}", field_id.0));
        mangled.push_str(&type_mangle(definitions, field_type));
    }
}
