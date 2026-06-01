use cx_ast::ast::expression::{CXExpression, CXInitIndex};
use cx_mir::{
    mir::{
        data::{MIRType, MIRTypeKind},
        expression::{MIRExpressionKind, StructInitialization},
    },
    program::EnvironmentNamespace,
};
use cx_tokens::TokenRange;
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        aggregate::fields::struct_field,
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};

pub fn typecheck_initializer_list(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    indices: &[CXInitIndex],
    to_type: Option<&MIRType>,
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
                )?
                .into_expression()?;

                if expression.token_range.is_none() {
                    expression.token_range = Some(token_range.clone());
                }

                Ok(expression)
            },
        ));
    };

    let to_type = env.mem_ref_inner(to_type).unwrap_or(to_type).clone();

    match &to_type.kind {
        MIRTypeKind::Array {
            inner_type,
            length,
        } => {
            let inner_type = env.resolve_type_id(inner_type).clone();
            typecheck_array_initializer(env, namespace, indices, &inner_type, Some(*length), &to_type)
        }

        MIRTypeKind::PointerTo {
            inner_type: inner, ..
        } => {
            let inner_type = env.resolve_type_id(inner).clone();
            typecheck_array_initializer(env, namespace, indices, &inner_type, None, &to_type)
        }

        MIRTypeKind::Structured { .. } => {
            typecheck_structured_initializer(env, namespace, expr, indices, &to_type)
        }

        _ => log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Cannot coerce initializer to type {}",
            to_type.display_with(&env.symbols)
        ),
    }
}

fn typecheck_array_initializer(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    indices: &[CXInitIndex],
    inner_type: &MIRType,
    size: Option<usize>,
    _to_type: &MIRType,
) -> CXResult<TypecheckResult> {
    for index in indices {
        if let Some(name) = &index.name {
            return log_typecheck_error!(
                env,
                Some(&TokenRange::default()),
                "Array initializer cannot have named indices, found: {name}"
            );
        }
    }

    if let Some(size) = size
        && indices.len() > size
    {
        return log_typecheck_error!(
            env,
            Some(&TokenRange::default()),
            "Too many elements in array initializer (expected {}, found {})",
            size,
            indices.len()
        );
    }

    let array_size = size.unwrap_or(indices.len());
    let array_type = MIRType::from(MIRTypeKind::Array {
        inner_type: env.intern_type(inner_type.clone()),
        length: array_size,
    });

    let elements = indices
        .iter()
        .map(|index| {
            typecheck_expr(env, namespace, &index.value, Some(inner_type))
                .and_then(TypecheckResult::into_expression)
        })
        .collect::<CXResult<_>>()?;

    Ok(TypecheckResult::new_base(
        array_type,
        MIRExpressionKind::ArrayInitializer {
            elements,
            element_type: inner_type.clone(),
        },
    ))
}

fn typecheck_structured_initializer(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    indices: &[CXInitIndex],
    to_type: &MIRType,
) -> CXResult<TypecheckResult> {
    let Some(fields) = to_type.aggregate_fields(&env.symbols) else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Expected a structured type for initializer, found {}",
            to_type.display_with(&env.symbols)
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
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Structured initializer has unexpected field: {name}"
                );
            };
            counter = found_index;
        }

        if counter >= fields.len() {
            return log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "Too many elements in struct initializer"
            );
        }

        if initialized_fields[counter] {
            return log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "Field '{}' initialized more than once",
                fields[counter].0
            );
        }

        let (field_name, field_type) = &fields[counter];
        let value = typecheck_expr(env, namespace, &index.value, Some(field_type))
            .and_then(|expr| std_rval_promotion(env, expr.into_expression()?))
            .and_then(|expr| implicit_cast(env, expr, field_type))?;

        let Some(struct_field_info) =
            struct_field(to_type, &env.symbols, field_name.as_str())
        else {
            return log_typecheck_error!(
                env,
                value.token_range.as_ref(),
                "Could not find field '{}' in type {}",
                field_name,
                to_type.display_with(&env.symbols)
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

    Ok(TypecheckResult::new_base(
        to_type.clone(),
        MIRExpressionKind::StructInitializer {
            struct_type: to_type.clone(),
            initializations,
        },
    ))
}
