use std::env::args;
use log::log;
use crate::log_error;
use crate::parse::ast::{ValueType, VarInitialization};
use crate::parse::verify::bytecode::{BytecodeBuilder, VirtualValue};
use crate::parse::verify::context::{FunctionPrototype, TypeMap, VerifyContext};

pub(crate) fn verify_fn_prototype(type_map: &TypeMap, mut prototype: FunctionPrototype) -> Option<FunctionPrototype> {
    prototype.return_type = get_intrinsic_type(type_map, &prototype.return_type)?.clone();

    let mut args = Vec::new();

    if matches!(prototype.return_type, ValueType::Structured { .. }) {
        args.push(VarInitialization {
            name: "__hidden_struct_pointer".to_string(),
            type_: prototype.return_type.clone()
        });
    }

    prototype.args = {
        for arg in prototype.args.into_iter() {
            let type_ = verify_type(type_map, arg.type_.clone())?;
            args.push(VarInitialization { name: arg.name.clone(), type_ });
        }

        args
    };

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
                    same_type(type_map, &f1.type_, &f2.type_))
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

pub(crate) fn struct_field_index(fields: &Vec<VarInitialization>, field_name: &str) -> Option<usize> {
    fields.iter()
        .position(|field| field.name == field_name)
        .or_else(|| {
            log_error!("Field {} not found in struct", field_name);
        })
}

pub(crate) fn struct_field_offset(context: &VerifyContext, builder: &mut BytecodeBuilder,
                                  fields: &Vec<VarInitialization>, field_name: &str) -> Option<usize> {
    let field_index = struct_field_index(fields, field_name)?;

    fields[0.. field_index]
        .iter()
        .map(|field| get_type_size(&context.type_map, &field.type_))
        .sum::<Option<usize>>()
}
