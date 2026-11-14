use cx_mir::{
    MIRFloatType, MIRFnSignature, MIRUndefined, MIRIntType, MIRType, MIRTypeID, MIRTypeKind, MIRTypeMetadata,
    ty::{interface::MTRegistry, registry::MIRTypeRegistry},
};
use cx_util::identifier::CXIdent;

pub(crate) fn declare(types: &MIRTypeRegistry, ty: MIRTypeID, declarator: &str) -> String {
    let definition = definition(types, ty);
    match definition.kind() {
        MIRTypeKind::PointerTo { inner } | MIRTypeKind::MemoryReference { inner } => {
            let pointer = join(&format!("*{}", qualifiers(definition.metadata(), true)), declarator);
            let pointer = match definition_kind(types, *inner) {
                MIRTypeKind::Array { .. }
                | MIRTypeKind::Undefined(MIRUndefined::Array { .. })
                | MIRTypeKind::Function { .. } => format!("({pointer})"),
                _ => pointer,
            };
            declare(types, *inner, &pointer)
        }
        MIRTypeKind::Array { length, inner } => declare(types, *inner, &format!("{declarator}[{length}]")),
        MIRTypeKind::Undefined(MIRUndefined::Array { element, .. }) => declare(types, *element, &format!("{declarator}[]")),
        MIRTypeKind::Function { signature } => {
            let declarator = format!("{declarator}({})", parameters(types, signature, false));
            declare(types, signature.return_type(), &declarator)
        }
        _ => {
            let specifier = format!("{}{}", qualifiers(definition.metadata(), false), specifier(types, ty));
            join(&specifier, declarator)
        }
    }
}

pub(crate) fn parameters(types: &MIRTypeRegistry, signature: &MIRFnSignature, named: bool) -> String {
    let mut params = signature
        .params()
        .iter()
        .enumerate()
        .filter(|(_, param)| !matches!(definition_kind(types, param.ty()), MIRTypeKind::Void))
        .map(|(index, param)| {
            let name = match named {
                true => param.name().map(CXIdent::as_string).unwrap_or(format!("arg{index}")),
                false => String::new(),
            };
            declare(types, param.ty(), &name)
        })
        .collect::<Vec<_>>();
    if signature.variadic() {
        params.push("...".into());
    }
    match params.is_empty() {
        true => "void".into(),
        false => params.join(", "),
    }
}

pub(crate) fn aggregate_name(types: &MIRTypeRegistry, ty: MIRTypeID) -> String {
    match types.debug_name(ty) {
        Some(name) => name.chars().map(|c| if c.is_ascii_alphanumeric() { c } else { '_' }).collect(),
        None => {
            let canonical = types.find(&MIRType::new(definition_kind(types, ty).clone())).unwrap_or(ty);
            format!("mir_type_{}", canonical.index())
        }
    }
}

pub(crate) fn aggregate_keyword(kind: &MIRTypeKind) -> Option<&'static str> {
    match kind {
        MIRTypeKind::Structured { .. } | MIRTypeKind::TaggedUnion { .. } => Some("struct"),
        MIRTypeKind::Union { .. } => Some("union"),
        MIRTypeKind::Undefined(undefined) => match undefined {
            MIRUndefined::Struct | MIRUndefined::TaggedUnion => Some("struct"),
            MIRUndefined::Union => Some("union"),
            MIRUndefined::Enum => Some("enum"),
            MIRUndefined::Array { .. } => None,
        },
        _ => None,
    }
}

pub(crate) fn definition_kind(types: &MIRTypeRegistry, ty: MIRTypeID) -> &MIRTypeKind {
    definition(types, ty).kind()
}

fn definition(types: &MIRTypeRegistry, ty: MIRTypeID) -> &MIRType {
    types.definition(ty).unwrap_or_else(|| panic!("header type {ty} is not in the registry"))
}

fn specifier(types: &MIRTypeRegistry, ty: MIRTypeID) -> String {
    let kind = definition_kind(types, ty);
    if let Some(keyword) = aggregate_keyword(kind) {
        return format!("{keyword} {}", aggregate_name(types, ty));
    }
    match kind {
        MIRTypeKind::Void => "void".into(),
        MIRTypeKind::Str => "char".into(),
        MIRTypeKind::Integer { ty } => match ty {
            MIRIntType::I1 => "bool",
            MIRIntType::I8 => "int8_t",
            MIRIntType::I16 => "int16_t",
            MIRIntType::I32 => "int32_t",
            MIRIntType::I64 => "int64_t",
            MIRIntType::I128 => "__int128",
        }
        .into(),
        MIRTypeKind::Float { ty } => match ty {
            MIRFloatType::F32 => "float",
            MIRFloatType::F64 => "double",
        }
        .into(),
        MIRTypeKind::Builtin { name, .. } => name.clone(),
        _ => unreachable!("{kind:?} is not a C type specifier"),
    }
}

fn qualifiers(metadata: MIRTypeMetadata, pointer: bool) -> String {
    [
        (MIRTypeMetadata::CONST, "const"),
        (MIRTypeMetadata::VOLATILE, "volatile"),
        (MIRTypeMetadata::RESTRICT, "restrict"),
    ]
    .into_iter()
    .filter(|(flag, _)| metadata.contains(*flag) && (pointer || *flag != MIRTypeMetadata::RESTRICT))
    .map(|(_, qualifier)| format!("{qualifier} "))
    .collect()
}

fn join(specifier: &str, declarator: &str) -> String {
    match (specifier.trim_end(), declarator) {
        (specifier, "") => specifier.to_string(),
        (specifier, declarator) if specifier.ends_with('*') => format!("{specifier}{declarator}"),
        (specifier, declarator) => format!("{specifier} {declarator}"),
    }
}
