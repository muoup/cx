use cx_parsing_data::ast::{CXExpr, CXInitIndex};
use cx_typechecker_data::mir::{
    expression::{MIRInstruction, MIRValue},
    program::MIRBaseMappings,
    types::{CXIntegerType, CXType, CXTypeKind},
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment, log_typecheck_error, type_checking::{binary_ops::{handle_assignment, struct_field}, casting::implicit_cast, typechecker::typecheck_expr},
};

pub fn typecheck_initializer_list(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    indices: &[CXInitIndex],
    to_type: Option<&CXType>,
) -> CXResult<MIRValue> {
    let Some(to_type) = to_type else {
        return log_typecheck_error!(env, expr, " Initializer lists must have an explicit type");
    };

    let to_type = match &to_type.kind {
        CXTypeKind::MemoryReference(inner) => inner.as_ref(),
        _ => to_type,
    };

    match &to_type.kind {
        CXTypeKind::Array {
            inner_type: _type,
            size,
        } => typecheck_array_initializer(env, base_data, indices, _type, Some(*size), to_type),

        CXTypeKind::PointerTo {
            inner_type: inner,
            sizeless_array: true,
            ..
        } => typecheck_array_initializer(env, base_data, indices, inner.as_ref(), None, to_type),

        CXTypeKind::Structured { .. } => {
            typecheck_structured_initializer(env, base_data, expr, indices, to_type)
        }

        _ => log_typecheck_error!(env, expr, " Cannot coerce initializer to type {to_type}"),
    }
}

fn typecheck_array_initializer(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    indices: &[CXInitIndex],
    inner_type: &CXType,
    size: Option<usize>,
    to_type: &CXType,
) -> CXResult<MIRValue> {
    for index in indices {
        if let Some(name) = &index.name {
            return log_typecheck_error!(
                env,
                &CXExpr::default(),
                "Array initializer cannot have named indices, found: {name}"
            );
        }
    }

    if let Some(size) = size {
        if indices.len() > size {
            return log_typecheck_error!(
                env,
                &CXExpr::default(),
                "Too many elements in array initializer (expected {}, found {})",
                size,
                indices.len()
            );
        }
    }

    let array_size = size.unwrap_or_else(|| indices.len());
    let array_type = CXTypeKind::Array {
        inner_type: Box::new(inner_type.clone()),
        size: array_size,
    }
    .into();

    let region = env.builder.new_register();
    env.builder
        .add_instruction(MIRInstruction::CreateStackRegion {
            result: region.clone(),
            _type: array_type,
        });

    let region_val = MIRValue::Register {
        register: region,
        _type: to_type.clone(),
    };

    for (i, index) in indices.iter().enumerate() {
        let value = typecheck_expr(env, base_data, &index.value, Some(inner_type))?;

        let element_ptr = env.builder.new_register();
        env.builder.add_instruction(MIRInstruction::ArrayGet {
            result: element_ptr.clone(),
            source: region_val.clone(),
            index: MIRValue::IntLiteral {
                value: i as i64,
                _type: CXIntegerType::I64,
                signed: true
            },
            element_type: inner_type.clone(),
        });

        env.builder.add_instruction(MIRInstruction::MemoryWrite {
            target: MIRValue::Register {
                register: element_ptr,
                _type: inner_type.clone().mem_ref_to(),
            },
            value,
        });
    }

    Ok(region_val)
}

fn typecheck_structured_initializer(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    indices: &[CXInitIndex],
    to_type: &CXType,
) -> CXResult<MIRValue> {
    let CXTypeKind::Structured { fields, .. } = &to_type.kind else {
        return log_typecheck_error!(
            env,
            expr,
            " Expected structured type for initializer, found: {to_type}"
        );
    };

    let region = env.builder.new_register();
    env.builder
        .add_instruction(MIRInstruction::CreateStackRegion {
            result: region.clone(),
            _type: to_type.clone(),
        });

    let region_val = MIRValue::Register {
        register: region,
        _type: to_type.clone(),
    };

    let mut counter = 0;
    let mut initialized_fields = vec![false; fields.len()];

    for index in indices.iter() {
        if let Some(name) = &index.name {
            let Some(found_index) = fields
                .iter()
                .position(|(field_name, _)| name.as_str() == field_name.as_str())
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Structured initializer has unexpected field: {name}"
                );
            };
            counter = found_index;
        }

        if counter >= fields.len() {
            return log_typecheck_error!(env, expr, "Too many elements in struct initializer");
        }

        if initialized_fields[counter] {
            return log_typecheck_error!(
                env,
                expr,
                "Field '{}' initialized more than once",
                fields[counter].0
            );
        }

        let (field_name, field_type) = &fields[counter];
        let value = typecheck_expr(env, base_data, &index.value, Some(field_type))
            .and_then(|v| implicit_cast(env, &index.value, v, field_type))?;
        
        let Some(struct_field) = struct_field(to_type, field_name.as_str()) else {
            return log_typecheck_error!(
                env,
                expr,
                " Could not find field '{}' in type {}",
                field_name,
                to_type
            );
        };
        
        let element_ptr = env.builder.new_register();
        env.builder.add_instruction(MIRInstruction::StructGet {
            result: element_ptr.clone(),
            source: region_val.clone(),
            field_index: struct_field.index,
            field_offset: struct_field.offset,
            struct_type: to_type.clone(),
        });
        
        let element_ptr_val = MIRValue::Register {
            register: element_ptr,
            _type: field_type.clone().mem_ref_to(),
        };
        
        handle_assignment(env, &element_ptr_val, &value, field_type)?;
        initialized_fields[counter] = true;

        if index.name.is_none() {
            counter += 1;
        }
    }

    Ok(region_val)
}
