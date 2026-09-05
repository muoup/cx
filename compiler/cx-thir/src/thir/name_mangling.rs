use cx_hir::registry::{ExportNameMode, GlobalSymbolRegistry};
use cx_namespace::{
    mangling::mangle_namespace_symbol,
    module::{NamespacePath, QualifiedName},
};

use crate::{
    thir::{
        data::{THIRType, THIRTypeKind},
        r#type::THIRField,
    },
    type_context::THIRTypeContext,
};

pub(crate) fn mangle_function_name(
    global_registry: &GlobalSymbolRegistry,
    namespace: &NamespacePath,
    name: &QualifiedName,
) -> String {
    if name.namespace.is_root()
        || global_registry.export_name_mode(namespace) == ExportNameMode::Root
    {
        return name.to_string();
    }

    return mangle_namespace_symbol(name);
}

pub(crate) fn mangle_type_name(definitions: &impl THIRTypeContext, ty: &THIRType) -> String {
    if let Some(name) = ty.strong_identifier() {
        let mut mangled = String::from("n");
        push_component(&mut mangled, name);
        return mangled;
    }

    match &ty.kind {
        THIRTypeKind::Integer { _type, signed } => {
            format!("i{}{}", if *signed { 's' } else { 'u' }, _type)
        }
        THIRTypeKind::Float { _type } => {
            format!("f{}", _type)
        }
        THIRTypeKind::Str => "s".to_owned(),
        THIRTypeKind::Undefined => "u".to_owned(),
        THIRTypeKind::Void => "v".to_owned(),
        THIRTypeKind::Unreachable => "n".to_owned(),
        THIRTypeKind::PointerTo { inner_type } => {
            let inner_type = definitions.resolve_type_id(*inner_type);
            let mut mangled = String::from("p");
            let inner_type = mangle_type_name(definitions, inner_type);
            push_component(&mut mangled, inner_type.as_str());
            mangled
        }
        THIRTypeKind::MemoryReference {
            inner_type,
            bitfield,
        } => {
            let mut mangled = String::from("r");
            if let Some(bitfield) = bitfield {
                mangled.push('1');
                push_component(&mut mangled, bitfield.bit_offset.to_string().as_str());
                push_component(&mut mangled, bitfield.bit_width.to_string().as_str());
                let storage_type = mangle_type_name(
                    definitions,
                    definitions.resolve_type_id(bitfield.storage_type),
                );
                push_component(&mut mangled, storage_type.as_str());
                push_component(&mut mangled, if bitfield.signed { "1" } else { "0" });
            } else {
                mangled.push('0');
            }
            let inner_type = mangle_type_name(definitions, definitions.resolve_type_id(*inner_type));
            push_component(&mut mangled, inner_type.as_str());
            mangled
        }
        THIRTypeKind::Opaque { size, alignment } => {
            let mut mangled = String::from("o");
            push_component(&mut mangled, size.to_string().as_str());
            push_component(&mut mangled, alignment.to_string().as_str());
            mangled
        }
        THIRTypeKind::Array {
            length: size,
            inner_type,
        } => {
            let mut mangled = String::from("a");
            let size = size.display_with(definitions).to_string();
            push_component(&mut mangled, size.as_str());
            let inner_type = mangle_type_name(definitions, definitions.resolve_type_id(*inner_type));
            push_component(&mut mangled, inner_type.as_str());
            mangled
        }
        THIRTypeKind::Function { signature } => {
            let mut mangled = String::from("f");
            let return_type = mangle_type_name(definitions, &signature.return_type);
            push_component(&mut mangled, return_type.as_str());
            push_component(&mut mangled, signature.params.len().to_string().as_str());
            for param in &signature.params {
                let param_type = mangle_type_name(definitions, &param._type);
                push_component(&mut mangled, param_type.as_str());
            }
            push_component(&mut mangled, if signature.var_args { "1" } else { "0" });
            mangled
        }
        THIRTypeKind::Structured { fields } => {
            let mut mangled = String::from("s");
            push_move_attributes(&mut mangled, ty);
            push_aggregate_fields(&mut mangled, definitions, fields);
            mangled
        }
        THIRTypeKind::Union { variants } => {
            let mut mangled = String::from("u");
            push_move_attributes(&mut mangled, ty);
            push_aggregate_fields(&mut mangled, definitions, variants);
            mangled
        }
        THIRTypeKind::TaggedUnion { variants } => {
            let mut mangled = String::from("t");
            push_move_attributes(&mut mangled, ty);
            push_aggregate_fields(&mut mangled, definitions, variants);
            mangled
        }
    }
}

fn push_move_attributes(mangled: &mut String, ty: &THIRType) {
    mangled.push(if ty.attributes.semantics.is_nocopy() {
        'C'
    } else {
        'c'
    });
    mangled.push(if ty.attributes.semantics.is_nodrop() {
        'D'
    } else {
        'd'
    });
    mangled.push(if ty.attributes.unsafe_move { 'M' } else { 'm' });
}

fn push_aggregate_fields(
    mangled: &mut String,
    definitions: &impl THIRTypeContext,
    fields: &[THIRField],
) {
    push_component(mangled, fields.len().to_string().as_str());
    for field in fields {
        let field_id = field.ty();
        if matches!(field, THIRField::Bitfield { .. }) {
            mangled.push('b');
        } else {
            mangled.push('f');
        }
        let field_type = definitions.resolve_type_id(field_id);
        let field_type = mangle_type_name(definitions, field_type);
        push_component(mangled, field_type.as_str());
    }
}

fn push_component(mangled: &mut String, component: &str) {
    mangled.push_str(component.len().to_string().as_str());
    mangled.push('_');
    mangled.push_str(component);
}
