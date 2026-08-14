use cx_hir::ast::expression::{HIRExpression, HIRInitIndex};
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    thir::{
        data::{THIRType, THIRTypeKind},
        expression::{StructInitialization, THIRExpressionKind},
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;

use crate::{
    environment::TypeEnvironment,
    type_checking::{
        aggregate::fields::struct_field, coercion::implicit::implicit_cast,
        result::TypecheckResult, typechecker::typecheck_expr,
    },
};

pub fn typecheck_initializer_list(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    indices: &[HIRInitIndex],
    to_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let Some(to_type) = to_type else {
        let expr = expr.clone();
        let indices = indices.to_vec();
        let token_range = expr.token_range().clone();

        return Ok(TypecheckResult::needs_expected_type(
            move |env, namespace, expected_type| {
                let mut expression = typecheck_initializer_list(
                    env,
                    namespace,
                    &expr,
                    &indices,
                    Some(expected_type),
                )
                .and_then(|v| v.standard_ready_coerce(env, &token_range))?;

                if !matches!(expression.token_range, TokenRange::Source { .. }) {
                    expression.token_range = token_range.clone();
                }

                Ok(expression)
            },
        ));
    };

    let to_type = env
        .symbols
        .mem_ref_inner(to_type)
        .unwrap_or(to_type)
        .clone();

    match &to_type.kind {
        THIRTypeKind::Array { inner_type, length } => {
            let inner_type = env.symbols.resolve_type_id(*inner_type).clone();
            typecheck_array_initializer(
                env,
                namespace,
                indices,
                &inner_type,
                Some(*length),
                &to_type,
            )
        }

        THIRTypeKind::PointerTo {
            inner_type: inner, ..
        } => {
            let inner_type = env.symbols.resolve_type_id(*inner).clone();
            typecheck_array_initializer(env, namespace, indices, &inner_type, None, &to_type)
        }

        THIRTypeKind::Structured { .. } => {
            typecheck_structured_initializer(env, namespace, expr, indices, &to_type)
        }

        _ => env.log_error(
            expr.token_range(),
            format!(
                "Cannot coerce initializer to type {}",
                to_type.display_with(&env.symbols)
            ),
        ),
    }
}

fn typecheck_array_initializer(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    indices: &[HIRInitIndex],
    inner_type: &THIRType,
    size: Option<usize>,
    _to_type: &THIRType,
) -> CXResult<TypecheckResult> {
    for index in indices {
        if let Some(name) = &index.name {
            return env.log_error(
                TokenRange::internal(),
                format!("Array initializer cannot have named indices, found: {name}"),
            );
        }
    }

    if let Some(size) = size
        && indices.len() > size
    {
        return env.log_error(
            TokenRange::internal(),
            format!(
                "Too many elements in array initializer (expected {}, found {})",
                size,
                indices.len()
            ),
        );
    }

    let array_size = size.unwrap_or(indices.len());
    let array_type = THIRType::from(THIRTypeKind::Array {
        inner_type: env.symbols.generate_type_id(inner_type.clone()),
        length: array_size,
    });

    let elements = indices
        .iter()
        .map(|index| {
            typecheck_expr(env, namespace, &index.value, Some(inner_type))
                .and_then(|v| v.standard_ready_coerce(env, index.value.token_range()))
        })
        .collect::<CXResult<_>>()?;

    Ok(TypecheckResult::new(
        array_type,
        THIRExpressionKind::ArrayInitializer {
            elements,
            element_type: inner_type.clone(),
        },
    ))
}

fn typecheck_structured_initializer(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    indices: &[HIRInitIndex],
    to_type: &THIRType,
) -> CXResult<TypecheckResult> {
    let Some(fields) = to_type.aggregate_fields(&env.symbols) else {
        return env.log_error(
            expr.token_range(),
            format!(
                "Expected a structured type for initializer, found {}",
                to_type.display_with(&env.symbols)
            ),
        );
    };
    let fields = fields.clone();

    let mut initializations = Vec::new();

    let mut counter = 0;
    let mut initialized_fields = vec![false; fields.len()];

    for index in indices.iter() {
        if let Some(name) = &index.name {
            let Some(found_index) = fields
                .iter()
                .position(|(field_name, _)| name.as_str() == field_name.as_str())
            else {
                return env.log_error(
                    expr.token_range(),
                    format!("Structured initializer has unexpected field: {name}"),
                );
            };
            counter = found_index;
        }

        if counter >= fields.len() {
            return env.log_error(
                expr.token_range(),
                "Too many elements in struct initializer".to_string(),
            );
        }

        if initialized_fields[counter] {
            return env.log_error(
                expr.token_range(),
                format!("Field '{}' initialized more than once", fields[counter].0),
            );
        }

        let (field_name, field_type) = &fields[counter];
        let value = typecheck_expr(env, namespace, &index.value, Some(field_type))
            .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
            .and_then(|v| implicit_cast(env, v, field_type))?;

        let Some(struct_field_info) = struct_field(&env.symbols, to_type, field_name.as_str())
        else {
            return env.log_error(
                value.token_range,
                format!(
                    "Could not find field '{}' in type {}",
                    field_name,
                    to_type.display_with(&env.symbols)
                ),
            );
        };

        initializations.push(StructInitialization {
            field_index: struct_field_info.index,
            value,
        });
        initialized_fields[counter] = true;

        if index.name.is_none() {
            counter += 1;
        }
    }

    Ok(TypecheckResult::new(
        to_type.clone(),
        THIRExpressionKind::StructInitializer {
            struct_type: to_type.clone(),
            initializations,
        },
    ))
}
