use std::collections::HashMap;
use std::fmt::Display;
use cx_util::log_error;
use crate::parse::ast::{CXFunctionPrototype, TypeMap};
use crate::parse::identifier::CXIdent;

#[derive(Debug, Clone, PartialEq)]
pub enum CXValType {
    Integer { bytes: u8, signed: bool },
    Float { bytes: u8 },
    Structured { fields: Vec<(String, CXValType)> },
    Unit,

    PointerTo(Box<CXValType>),
    MemoryReference(Box<CXValType>),
    Array {
        size: usize,
        _type: Box<CXValType>
    },
    Opaque {
        name: String,
        size: usize
    },
    Function {
        return_type: Box<CXValType>,
        args: Vec<CXValType>
    },

    Identifier(CXIdent)
}

pub fn is_structure(type_map: &TypeMap, val_type: &CXValType) -> bool {
    matches!(get_intrinsic_type(type_map, val_type), Some(CXValType::Structured { .. }))
}

pub fn same_type(type_map: &TypeMap, t1: &CXValType, t2: &CXValType) -> bool {
    match (t1, t2) {
        (CXValType::Identifier(name1),
            CXValType::Identifier(name2))
        if name1 == name2 =>
            true,

        (CXValType::Identifier(name1), _) =>
            same_type(type_map, &type_map.get(name1.as_str()).unwrap(), t2),

        (_, CXValType::Identifier(name2)) =>
            same_type(type_map, t1, &type_map.get(name2.as_str()).unwrap()),

        (CXValType::Array { _type: t1_type, .. },
            CXValType::Array { _type: t2_type, .. }) =>
            same_type(type_map, t1_type, t2_type),

        (CXValType::PointerTo(t1_type),
            CXValType::PointerTo(t2_type)) =>
            same_type(type_map, t1_type, t2_type),

        (CXValType::Structured { fields: t1_fields },
            CXValType::Structured { fields: t2_fields }) => {
            t1_fields.iter().zip(t2_fields.iter())
                .all(|(f1, f2)|
                    same_type(type_map, &f1.1, &f2.1))
        },

        (CXValType::Integer { bytes: t1_bytes, signed: t1_signed },
            CXValType::Integer { bytes: t2_bytes, signed: t2_signed }) =>
            t1_bytes == t2_bytes && t1_signed == t2_signed,

        (CXValType::Float { bytes: t1_bytes },
            CXValType::Float { bytes: t2_bytes }) =>
            t1_bytes == t2_bytes,

        (CXValType::Unit, CXValType::Unit) =>
            true,

        _ => false,
    }
}

pub fn get_intrinsic_type<'a>(type_map: &'a TypeMap, type_: &'a CXValType) -> Option<&'a CXValType> {
    match type_ {
        CXValType::Identifier(name)
        => type_map.get(name.as_str()),

        _ => Some(type_)
    }
}

pub fn get_type_size(type_map: &TypeMap, type_: &CXValType) -> Option<usize> {
    match type_ {
        CXValType::Unit => Some(0),

        CXValType::Float { bytes } => Some(*bytes as usize),
        CXValType::Integer { bytes, .. } => Some(*bytes as usize),
        CXValType::MemoryReference(inner) =>
            get_type_size(type_map, inner.as_ref()),

        CXValType::Array { _type, size }
        => Some(get_type_size(type_map, _type)? * size),

        CXValType::Structured { fields } =>
            fields.iter()
                .map(|field| get_type_size(type_map, &field.1))
                .sum(),

        CXValType::PointerTo(_)
        | CXValType::Function { .. } => Some(8),
        CXValType::Opaque { size, .. } => Some(*size),
        CXValType::Identifier(name) =>
            type_map.get(name.as_str())
                .map(|_type| get_type_size(type_map, _type))
                .flatten()
    }
}

pub(crate) fn struct_field_index(fields: &[(String, CXValType)], field_name: &str) -> Option<usize> {
    fields.iter()
        .position(|field| field.0 == field_name)
        .or_else(|| {
            log_error!("Field {} not found in struct", field_name);
        })
}

pub(crate) fn struct_field_offset(type_map: &TypeMap, fields: &[(String, CXValType)],
                                  field_name: &str) -> Option<usize> {
    let field_index = struct_field_index(fields, field_name)?;

    fields[0.. field_index]
        .iter()
        .map(|field| get_type_size(type_map, &field.1))
        .sum::<Option<usize>>()
}

pub struct StructAccessRecord {
    pub field_type: CXValType,
    pub field_offset: usize,
    pub field_index: usize,
    pub field_name: String
}

pub fn struct_field_access(
    type_map: &TypeMap,
    type_: &CXValType,
    field: &str
) -> Option<StructAccessRecord> {
    let CXValType::Structured { fields } = get_intrinsic_type(type_map, type_)? else {
        log_error!("Cannot access field {field} of non-structured type {type_}");
    };

    let mut offset = 0;

    for (i, (name, ty)) in fields.iter().enumerate() {
        if name == field {
            return Some(
                StructAccessRecord {
                    field_type: ty.clone(),
                    field_offset: offset,
                    field_index: i,
                    field_name: name.clone()
                }
            );
        }

        offset += get_type_size(type_map, ty)?;
    }

    None
}

pub fn prototype_to_type(prototype: &CXFunctionPrototype) -> Option<CXValType> {
    let return_type = prototype.return_type.clone();
    let args = prototype.parameters.iter()
        .cloned()
        .map(|param| param.type_)
        .collect::<Vec<_>>();

    Some(CXValType::Function {
        return_type: Box::new(return_type),
        args
    })
}

pub fn struct_field_type(
    type_map: &TypeMap,
    type_: &CXValType,
    field: &str
) -> Option<CXValType> {
    let CXValType::Structured { fields } = get_intrinsic_type(type_map, type_)? else {
        log_error!("Cannot access field {field} of non-structured type {type_}");
    };

    fields.iter()
        .find(|(name, _)| name == field)
        .map(|(_, ty)| ty.clone())
}
