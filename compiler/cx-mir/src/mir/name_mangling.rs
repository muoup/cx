use crate::mir::data::{MIRType, MIRTypeId, MIRTypeKind};
use crate::mir::r#type::MIRField;
use crate::type_context::MIRTypeContext;
use cx_ast::registry::{ExportNameMode, GlobalSymbolRegistry};
use cx_util::namespace::QualifiedName;

pub fn mangle_qualified_name(
    global_registry: &GlobalSymbolRegistry,
    name: &QualifiedName,
) -> String {
    if name.namespace.is_root()
        || global_registry.export_name_mode(&name.namespace) == ExportNameMode::Root
    {
        return name.name.as_str().to_string();
    }

    mangle_namespace_symbol(name)
}

fn mangle_namespace_symbol(name: &QualifiedName) -> String {
    let mut mangled = String::from("_N");

    for segment in name.namespace.segments() {
        mangled.push('_');
        mangled.push_str(segment.as_str());
    }

    mangled.push('_');
    mangled.push_str(name.name.as_str());
    mangled
}

pub fn base_mangle_templated_name<'a>(
    definitions: &impl MIRTypeContext,
    name: &str,
    template_args: impl ExactSizeIterator<Item = &'a MIRType>,
) -> String {
    let mut mangled = format!("_T{}{}", template_args.len(), name);
    for arg in template_args {
        mangled.push_str(type_mangle(definitions, arg).as_str());
    }
    mangled
}

pub fn base_mangle_member(
    definitions: &impl MIRTypeContext,
    name: &str,
    member_type: &MIRType,
) -> String {
    format!("_M{}_{}", type_mangle(definitions, member_type), name)
}

pub(crate) fn type_mangle(definitions: &impl MIRTypeContext, ty: &MIRType) -> String {
    if let Some(name) = ty.strong_identifier() {
        return format!("n{}", name);
    }

    match &ty.kind {
        MIRTypeKind::Integer { _type, signed } => {
            format!(
                "{}{}",
                 if *signed { 's' } else { 'u' },
                 _type
            )
        }
        MIRTypeKind::Float { _type } => {
            format!(
                "{}{}",
                 'f',
                 _type
            )
        }
        MIRTypeKind::Str => {
            "_str".to_owned()
        }
        MIRTypeKind::Undefined => {
            "X".to_owned()
        }
        MIRTypeKind::Unit => {
            "v".to_owned()
        }
        MIRTypeKind::PointerTo { inner_type } => {
            let inner_type = definitions.resolve_type_id(*inner_type);

            format!(
                "P{}",
                type_mangle(definitions, inner_type)
            )
        }
        MIRTypeKind::MemoryReference {
            inner_type,
            bitfield,
        } => {
            format!(
                "R{}{}",
                bitfield.as_ref()
                    .map(
                        |bitfield| format!("b{}_{}}}", bitfield.bit_offset, bitfield.bit_width)
                    )
                    .unwrap_or("".to_owned()),
                type_mangle(definitions, definitions.resolve_type_id(*inner_type))
            )
        }
        MIRTypeKind::Opaque { size } => {
            format!("O{}", size)
        }
        MIRTypeKind::Array {
            length: size,
            inner_type,
        } => {
            format!("A{}_{}", size, type_mangle(definitions, definitions.resolve_type_id(*inner_type)))
        }
        MIRTypeKind::Function { signature } => {
            format!(
                "F{}{}{}{}",
                type_mangle(definitions, &signature.return_type),
                signature.params.len(),
                signature.params.iter().map(|param| type_mangle(definitions, &param._type)).collect::<String>(),
                if signature.var_args { 'V' } else { 'v' }
            )
        }
        MIRTypeKind::Structured { fields } => {
            let mut mangled =
                format!(
                    "S{}",
                    fields.len()
                );
            push_move_attributes(&mut mangled, ty);
            push_aggregate_fields(&mut mangled, definitions, fields);
            mangled
        }
        MIRTypeKind::Union { variants } => {
            let mut mangled =
                format!(
                    "U{}",
                    variants.len()
                );
            push_move_attributes(&mut mangled, ty);
            push_aggregate_fields(&mut mangled, definitions, variants);
            mangled
        }
        MIRTypeKind::TaggedUnion { variants } => {
            let mut mangled =
                format!(
                    "T{}",
                    variants.len()
                );
            push_move_attributes(&mut mangled, ty);
            push_aggregate_fields(&mut mangled, definitions, variants);
            mangled
        }
    }
}

fn push_move_attributes(mangled: &mut String, ty: &MIRType) {
    mangled.push(if ty.move_attributes.nocopy { 'C' } else { 'c' });
    mangled.push(if ty.move_attributes.nodrop { 'D' } else { 'd' });
}

fn push_aggregate_fields(
    mangled: &mut String,
    definitions: &impl MIRTypeContext,
    fields: &[MIRField],
) {
    mangled.push('f');
    mangled.push_str(&fields.len().to_string());
    mangled.push('_');
    for field in fields {
        let field_id = field.ty();
        if matches!(field, MIRField::Bitfield { .. }) {
            mangled.push('b');
        }
        let field_type = definitions.resolve_type_id(field_id);
        mangled.push_str(&type_mangle(definitions, field_type));
    }
}
