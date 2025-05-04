use crate::log_error;
use crate::parse::pass_bytecode::builder::{BytecodeBuilder, ValueID, VirtualInstruction};
use crate::parse::pass_ast::TypeMap;
use crate::parse::value_type::CXValType;

pub(crate) fn same_type(type_map: &TypeMap, t1: &CXValType, t2: &CXValType) -> bool {
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

pub(crate) fn get_intrinsic_type<'a>(type_map: &'a TypeMap, type_: &'a CXValType) -> Option<&'a CXValType> {
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

pub(crate) fn struct_field_offset(builder: &mut BytecodeBuilder, fields: &[(String, CXValType)],
                                  field_name: &str) -> Option<usize> {
    let field_index = struct_field_index(fields, field_name)?;

    fields[0.. field_index]
        .iter()
        .map(|field| get_type_size(&builder.type_map, &field.1))
        .sum::<Option<usize>>()
}

pub(crate) fn implicit_casting(
    builder: &mut BytecodeBuilder,
    value: ValueID,
    from_type: &CXValType,
    to_type: &CXValType
) -> Option<ValueID> {
    match (get_intrinsic_type(&builder.type_map, from_type).cloned()?,
           get_intrinsic_type(&builder.type_map, to_type).cloned()?) {
        (x, y) if x == y => {
            Some(value)
        }

        (CXValType::Integer { bytes: lb, .. }, CXValType::Integer { bytes: rb, .. })
            if lb > rb => {

            builder.add_instruction(
                VirtualInstruction::Trunc {
                    value,
                },
                to_type.clone()
            )
        }

        (CXValType::Integer { .. }, CXValType::Integer { signed: true, .. }) => {
            builder.add_instruction(
                VirtualInstruction::SExtend {
                    value,
                },
                to_type.clone()
            )
        },

        (CXValType::Integer { .. }, CXValType::Integer { signed: false, .. }) => {
            builder.add_instruction(
                VirtualInstruction::ZExtend {
                    value,
                },
                to_type.clone()
            )
        },

        _ => todo!("Implicit cast from {from_type} to {to_type}")
    }
}