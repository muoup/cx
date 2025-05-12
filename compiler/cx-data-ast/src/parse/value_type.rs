use std::collections::HashMap;
use std::fmt::Display;
use cx_util::log_error;
use crate::parse::ast::{CXFunctionPrototype, TypeMap};
use crate::parse::identifier::CXIdent;
use crate::parse::parser::ParserData;

pub type CXTypeSpecifier = u8;

pub const CX_CONST: CXTypeSpecifier = 1 << 0;
pub const CX_VOLATILE: CXTypeSpecifier = 1 << 1;
pub const CX_RESTRICT: CXTypeSpecifier = 1 << 2;
pub const CX_THREAD_LOCAL: CXTypeSpecifier = 1 << 3;
pub const CX_UNION: CXTypeSpecifier = 1 << 4;

#[derive(Debug, Clone)]
pub struct CXValType {
    pub specifiers: CXTypeSpecifier,
    pub internal_type: CXTypeUnion
}

#[derive(Debug, Clone)]
pub enum CXTypeUnion {
    Integer { bytes: u8, signed: bool },
    Float { bytes: u8 },
    Structured {
        name: Option<CXIdent>,
        fields: Vec<(String, CXValType)>
    },
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

impl CXTypeUnion {
    pub fn to_val_type(self) -> CXValType {
        CXValType::new(
            0,
            self
        )
    }
}

impl CXValType {
    pub fn unit() -> Self {
        CXValType {
            specifiers: 0,
            internal_type: CXTypeUnion::Unit
        }
    }

    pub fn new(specifiers: CXTypeSpecifier, underlying_type: CXTypeUnion) -> Self {
        CXValType {
            specifiers,
            internal_type: underlying_type
        }
    }

    pub fn add_specifier(&self, specifier: CXTypeSpecifier) -> Self {
        CXValType {
            specifiers: self.specifiers | specifier,
            internal_type: self.internal_type.clone()
        }
    }

    pub fn remove_specifier(&self, specifier: CXTypeSpecifier) -> Self {
        CXValType {
            specifiers: self.specifiers & !specifier,
            internal_type: self.internal_type.clone()
        }
    }

    pub fn get_specifier(&self, specifier: CXTypeSpecifier) -> bool {
        self.specifiers & specifier == specifier
    }

    pub fn intrinsic_type<'a>(&'a self, type_map: &'a TypeMap) -> Option<&'a CXTypeUnion> {
        get_intrinsic_type(type_map, self)
    }
        pub fn is_structure(&self, type_map: &TypeMap) -> bool {
        matches!(get_intrinsic_type(type_map, self), Some(CXTypeUnion::Structured { .. }))
    }

    pub fn is_void(&self, type_map: &TypeMap) -> bool {
        matches!(self.intrinsic_type(type_map), Some(CXTypeUnion::Unit))
    }

    pub fn intrin_eq(&self, other: &CXValType, type_map: &TypeMap) -> bool {
        same_type(type_map, self, other)
    }

    pub fn is_intrinsic(&self, intrin: &CXTypeUnion, type_map: &TypeMap) -> bool {
        matches!(get_intrinsic_type(type_map, self), intrin)
    }

    pub fn size(&self, type_map: &TypeMap) -> Option<usize> {
        get_type_size(type_map, self)
    }

    pub fn pointer_to(self) -> Self {
        CXValType {
            specifiers: 0,
            internal_type: CXTypeUnion::PointerTo(Box::new(self))
        }
    }

    pub fn deref(self) -> Self {
        CXValType {
            specifiers: 0,
            internal_type: CXTypeUnion::MemoryReference(Box::new(self))
        }
    }
}

pub fn is_structure(type_map: &TypeMap, val_type: &CXValType) -> bool {
    matches!(get_intrinsic_type(type_map, val_type), Some(CXTypeUnion::Structured { .. }))
}

fn same_type(type_map: &TypeMap, t1: &CXValType, t2: &CXValType) -> bool {
    match (&t1.internal_type, &t2.internal_type) {
        (CXTypeUnion::Identifier(name1),
            CXTypeUnion::Identifier(name2))
        if name1 == name2 =>
            true,

        (CXTypeUnion::Identifier(name1), _) =>
            same_type(type_map, &type_map.get(name1.as_str()).unwrap(), t2),

        (_, CXTypeUnion::Identifier(name2)) =>
            same_type(type_map, t1, &type_map.get(name2.as_str()).unwrap()),

        (CXTypeUnion::Array { _type: t1_type, .. },
         CXTypeUnion::Array { _type: t2_type, .. }) =>
            same_type(type_map, t1_type, t2_type),

        (CXTypeUnion::PointerTo(t1_type),
         CXTypeUnion::PointerTo(t2_type)) =>
            same_type(type_map, t1_type, t2_type),

        (CXTypeUnion::Structured { fields: t1_fields, .. },
         CXTypeUnion::Structured { fields: t2_fields, .. }) => {
            t1_fields.iter().zip(t2_fields.iter())
                .all(|(f1, f2)|
                    same_type(type_map, &f1.1, &f2.1))
        },

        (CXTypeUnion::Function { return_type: ret1, args: args1 },
         CXTypeUnion::Function { return_type: ret2, args: args2 }) =>
            same_type(type_map, ret1, ret2) &&
                args1.iter().zip(args2.iter())
                    .all(|(a1, a2)| same_type(type_map, a1, a2)),

        (CXTypeUnion::Integer { bytes: t1_bytes, signed: t1_signed },
            CXTypeUnion::Integer { bytes: t2_bytes, signed: t2_signed }) =>
            t1_bytes == t2_bytes && t1_signed == t2_signed,

        (CXTypeUnion::Float { bytes: t1_bytes },
            CXTypeUnion::Float { bytes: t2_bytes }) =>
            t1_bytes == t2_bytes,

        (CXTypeUnion::Unit, CXTypeUnion::Unit) =>
            true,

        _ => false,
    }
}

pub fn get_intrinsic_type<'a>(type_map: &'a TypeMap, type_: &'a CXValType) -> Option<&'a CXTypeUnion> {
    match &type_.internal_type {
        CXTypeUnion::Identifier(name)
            => type_map.get(name.as_str())
                .and_then(|_type| get_intrinsic_type(type_map, _type)),

        _ => Some(&type_.internal_type)
    }
}

pub fn get_type_size(type_map: &TypeMap, type_: &CXValType) -> Option<usize> {
    match &type_.internal_type {
        CXTypeUnion::Unit => Some(0),

        CXTypeUnion::Float { bytes } => Some(*bytes as usize),
        CXTypeUnion::Integer { bytes, .. } => Some(*bytes as usize),
        CXTypeUnion::MemoryReference(inner) =>
            get_type_size(type_map, inner.as_ref()),

        CXTypeUnion::Array { _type, size }
        => Some(get_type_size(type_map, _type)? * size),

        CXTypeUnion::Structured { fields, .. } =>
            fields.iter()
                .map(|field| get_type_size(type_map, &field.1))
                .sum(),

        CXTypeUnion::PointerTo(_)
        | CXTypeUnion::Function { .. } => Some(8),

        CXTypeUnion::Opaque { size, .. } => Some(*size),
        CXTypeUnion::Identifier(name) =>
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
        .map(|(_, type_)| get_type_size(type_map, type_))
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
    let CXTypeUnion::Structured { fields, .. } = get_intrinsic_type(type_map, type_)? else {
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

    Some(
        CXValType::new(
            0,
            CXTypeUnion::Function {
                return_type: Box::new(return_type),
                args
            }
        )
    )
}

pub fn struct_field_type(
    type_map: &TypeMap,
    type_: &CXValType,
    field: &str
) -> Option<CXValType> {
    let CXTypeUnion::Structured { fields, .. }
        = &get_intrinsic_type(type_map, type_)? else {
        log_error!("Cannot access field {field} of non-structured type {type_}");
    };

    fields.iter()
        .find(|(name, _)| name == field)
        .map(|(_, ty)| ty.clone())
}
