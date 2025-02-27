use crate::log_error;
use crate::parse::ast::{ValueType, VarInitialization};
use crate::parse::verify::context::{FunctionPrototype, TypeMap, VerifyContext};

pub(crate) fn verify_fn_prototype(type_map: &TypeMap, mut prototype: FunctionPrototype) -> Option<FunctionPrototype> {
    prototype.return_type = verify_type(type_map, prototype.return_type.clone())?;
    prototype.args = prototype.args
        .into_iter()
        .map(|arg| {
            let type_ = verify_type(type_map, arg.type_.clone())?;
            Some(VarInitialization { name: arg.name, type_ })
        })
        .collect::<Option<Vec<VarInitialization>>>()?;

    Some(prototype)
}

pub(crate) fn verify_type(type_map: &TypeMap, val_type: ValueType) -> Option<ValueType> {
    match val_type {
        ValueType::Array { size, _type } => {
            let _type = Box::new(verify_type(type_map, *_type)?);
            Some(ValueType::Array { size, _type })
        },

        ValueType::PointerTo(_type) => {
            let _type = Box::new(verify_type(type_map, *_type)?);
            Some(ValueType::PointerTo(_type))
        },

        ValueType::Structured { fields } => {
            let fields = fields.into_iter()
                .map(|field| {
                    let type_ = verify_type(type_map, field.type_)?;
                    Some(VarInitialization { name: field.name, type_ })
                })
                .collect::<Option<Vec<VarInitialization>>>()?;

            Some(ValueType::Structured { fields })
        },

        ValueType::Identifier(name) => {
            if type_map.get(name.as_str()).is_none() {
                log_error!("Type {} not found", name);
            };

            Some(ValueType::Identifier(name))
        },

        ValueType::Unit
        | ValueType::Float { .. }
        | ValueType::Integer { .. } => Some(val_type),
    }
}

pub(crate) fn same_type(type_map: &TypeMap, t1: &ValueType, t2: &ValueType) -> bool {
    match (t1, t2) {
        (ValueType::Identifier(name1),
         ValueType::Identifier(name2))
        if name1 == name2 =>
            true,

        (ValueType::Identifier(name1), _) =>
            same_type(type_map, &type_map.get(name1.as_str()).unwrap(), t2),

        (_, ValueType::Identifier(name2)) =>
            same_type(type_map, t1, &type_map.get(name2.as_str()).unwrap()),

        (ValueType::Array { _type: t1_type, .. },
         ValueType::Array { _type: t2_type, .. }) =>
            same_type(type_map, t1_type, t2_type),

        (ValueType::PointerTo(t1_type),
         ValueType::PointerTo(t2_type)) =>
            same_type(type_map, t1_type, t2_type),

        (ValueType::Structured { fields: t1_fields },
         ValueType::Structured { fields: t2_fields }) => {
            t1_fields.iter().zip(t2_fields.iter())
                .all(|(f1, f2)|
                    f1.name == f2.name && same_type(type_map, &f1.type_, &f2.type_))
        },

        (ValueType::Integer { bytes: t1_bytes, signed: t1_signed },
         ValueType::Integer { bytes: t2_bytes, signed: t2_signed }) =>
            t1_bytes == t2_bytes && t1_signed == t2_signed,

        (ValueType::Float { bytes: t1_bytes },
         ValueType::Float { bytes: t2_bytes }) =>
            t1_bytes == t2_bytes,

        (ValueType::Unit, ValueType::Unit) =>
            true,

        _ => false,
    }
}

pub(crate) fn get_instrinic_type<'a>(type_map: &'a TypeMap, type_: &'a ValueType) -> Option<&'a ValueType> {
    match type_ {
        ValueType::Identifier(name)
            => type_map.get(name),

        _ => Some(type_)
    }
}

pub(crate) fn get_type_size(type_map: &TypeMap, type_: &ValueType) -> Option<usize> {
    match type_ {
        ValueType::Float { bytes } => Some(*bytes as usize),
        ValueType::Integer { bytes, .. } => Some(*bytes as usize),

        ValueType::Array { _type, size }
            => Some(get_type_size(type_map, _type)? * size),

        ValueType::Structured { fields } =>
            fields.iter()
                .map(|field| get_type_size(type_map, &field.type_))
                .sum(),

        ValueType::Unit => Some(0),
        ValueType::PointerTo(_) => Some(8),
        ValueType::Identifier(name) =>
            type_map.get(name)
                .map(|_type| get_type_size(type_map, _type))
                .flatten()
    }
}