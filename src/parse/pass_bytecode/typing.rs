use std::env::args;
use log::log;
use crate::log_error;
use crate::parse::pass_bytecode::builder::{BytecodeBuilder, VirtualValue};
use crate::parse::pass_molded::TypeMap;
use crate::parse::value_type::ValueType;

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
                    same_type(type_map, &f1.1, &f2.1))
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

pub(crate) fn get_intrinsic_type<'a>(type_map: &'a TypeMap, type_: &'a ValueType) -> Option<&'a ValueType> {
    match type_ {
        ValueType::Identifier(name)
            => type_map.get(name),

        _ => Some(type_)
    }
}

pub fn get_type_size(type_map: &TypeMap, type_: &ValueType) -> Option<usize> {
    match type_ {
        ValueType::Float { bytes } => Some(*bytes as usize),
        ValueType::Integer { bytes, .. } => Some(*bytes as usize),

        ValueType::Array { _type, size }
            => Some(get_type_size(type_map, _type)? * size),

        ValueType::Structured { fields } =>
            fields.iter()
                .map(|field| get_type_size(type_map, &field.1))
                .sum(),

        ValueType::Unit => Some(0),
        ValueType::PointerTo(_) => Some(8),
        ValueType::Opaque { size, .. } => Some(*size),
        ValueType::Identifier(name) =>
            type_map.get(name)
                .map(|_type| get_type_size(type_map, _type))
                .flatten()
    }
}

pub(crate) fn struct_field_index(fields: &[(String, ValueType)], field_name: &str) -> Option<usize> {
    fields.iter()
        .position(|field| field.0 == field_name)
        .or_else(|| {
            log_error!("Field {} not found in struct", field_name);
        })
}

pub(crate) fn struct_field_offset(builder: &mut BytecodeBuilder, fields: &[(String, ValueType)],
                                  field_name: &str) -> Option<usize> {
    let field_index = struct_field_index(fields, field_name)?;

    fields[0.. field_index]
        .iter()
        .map(|field| get_type_size(&builder.type_map, &field.1))
        .sum::<Option<usize>>()
}
