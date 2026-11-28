use chl_hmir::ty::{HMIRTypeID, HMIRTypeKind, aggregate::HMIRFieldDef, context::HMIRTypeContext};

use crate::{
    env::HMIREnvironment,
    value::{
        HMIRApplication, HMIRValue, HMIRValueKind,
        address::{Addressable, HMIRAddress},
        immediate::HMIRImmValue,
        staged::QuoteRef,
    },
};

pub fn format_value(
    env: &HMIREnvironment,
    value: &HMIRValue,
    type_name: &mut impl FnMut(HMIRTypeID) -> String,
) -> String {
    match value.kind() {
        HMIRValueKind::Immediate(value_kind) => immediate(env, value.ty(), value_kind, type_name),
        HMIRValueKind::Application(call) => {
            application(call, |value| format_value(env, value, type_name))
        }
        HMIRValueKind::Quote(code) => quote(code),
        HMIRValueKind::Reference(address) => offset(address.base().to_string(), address.offset()),
        HMIRValueKind::Register(register) => register.to_string(),
    }
}

fn immediate(
    env: &HMIREnvironment,
    ty: HMIRTypeID,
    value: &HMIRImmValue,
    type_name: &mut impl FnMut(HMIRTypeID) -> String,
) -> String {
    match value {
        HMIRImmValue::Unit => "()".into(),
        HMIRImmValue::Address(address) => address_name(env, address),
        HMIRImmValue::Application(call) => {
            application(call, |arg| immediate(env, arg.ty(), arg.value(), type_name))
        }
        HMIRImmValue::Int(value) => value.to_string(),
        HMIRImmValue::Float(value) => value.to_string(),
        HMIRImmValue::Aggregate { fields } => {
            let items = match env.resolve_kind(ty) {
                HMIRTypeKind::Array { element, .. } => fields
                    .iter()
                    .map(|item| immediate(env, *element, item, type_name))
                    .collect::<Vec<_>>(),
                HMIRTypeKind::Struct {
                    fields: declarations,
                } => fields
                    .iter()
                    .zip(declarations)
                    .map(|(item, field)| {
                        let item = immediate(env, *field.ty(), item, type_name);
                        match field.name() {
                            Some(name) => format!(".{name} = {item}"),
                            None => item,
                        }
                    })
                    .collect(),
                _ => return format!("<aggregate {}>", type_name(ty)),
            };
            format!("{{{}}}", items.join(", "))
        }
        HMIRImmValue::Union { variant, payload } => {
            let Some(field) = variant_field(env, ty, *variant) else {
                return format!("<variant {variant} of {}>", type_name(ty));
            };
            let payload = match payload.as_ref() {
                HMIRImmValue::Unit => String::new(),
                payload => immediate(env, *field.ty(), payload, type_name),
            };
            let name = field
                .name()
                .map_or_else(|| variant.to_string(), ToString::to_string);
            format!("{name}({payload})")
        }
        HMIRImmValue::Quote(code) => quote(code),
        HMIRImmValue::Constructor { ty, variant } => {
            let name = variant_field(env, *ty, *variant)
                .and_then(|field| field.name())
                .map_or_else(|| variant.to_string(), ToString::to_string);
            format!("{}::{name}", type_name(*ty))
        }
        HMIRImmValue::Type(ty) => type_name(*ty),
        HMIRImmValue::Opaque => format!("<unknown {}>", type_name(ty)),
    }
}

fn variant_field<'a>(
    env: &'a HMIREnvironment,
    ty: HMIRTypeID,
    index: usize,
) -> Option<&'a HMIRFieldDef<HMIRTypeID>> {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Union { fields } | HMIRTypeKind::TaggedUnion { fields } => fields.get(index),
        _ => None,
    }
}

fn application<V>(call: &HMIRApplication<V>, mut name: impl FnMut(&V) -> String) -> String {
    let callee = name(&call.callee);
    let args = call
        .args
        .iter()
        .map(|arg| arg.as_ref().map_or_else(|| "_".into(), &mut name));
    format!("{callee}({})", args.collect::<Vec<_>>().join(", "))
}

fn quote<V>(code: &QuoteRef<V>) -> String {
    let code = code.get();
    format!("emit <{}:{}>", code.source(), code.root())
}

fn address_name(env: &HMIREnvironment, address: &HMIRAddress<Addressable>) -> String {
    let base = match address.base() {
        Addressable::Def(def) => env.symbols().resolve_definition(*def).name().to_string(),
        Addressable::Label { function, label } => format!("&&{function}::{label}"),
        Addressable::Literal(text) => format!("{text:?}"),
    };
    offset(base, address.offset())
}

fn offset(base: String, bytes: i64) -> String {
    match bytes {
        0 => base,
        _ => format!("{base}{bytes:+}"),
    }
}
