use cx_log::catalogue::typecheck as catalogue;
use std::collections::{HashMap, HashSet};

use crate::{
    environment::TypeEnvironment,
    type_checking::{
        result::{BindingPlaceKind, TypecheckResult},
        typechecker::typecheck_expr,
    },
};
use cx_hir::ast::{
    expression::{HIRExpression, HIRUnpackBinding},
    modifiers::HIR_CONST,
};
use cx_log::CXResult;
use cx_namespace::module::NamespacePath;
use cx_namespace::module::QualifiedName;
use cx_thir::{
    thir::{
        data::{THIRType, THIRTypeKind},
        expression::{THIRExpression, THIRExpressionKind, THIRLocalID, THIRUnpackBinding},
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;

pub(crate) fn typecheck_move(
    env: &mut TypeEnvironment,
    _: &NamespacePath,
    inner: TypecheckResult,
    inner_expr: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let binding = inner.binding().cloned();
    let inner_val = inner.standard_ready_coerce(env, inner_expr.token_range())?;

    if !inner_val._type.is_memory_reference() {
        return Ok(TypecheckResult::from(inner_val));
    }

    let Some(binding) = binding else {
        return env.log_error(
            inner_expr.token_range(),
            &catalogue::MOVE_EXPRESSIONS_CAN_CURRENTLY_ONLY_BE_APPLIED_TO_STACK_VARIABLE,
            (),
        );
    };

    if binding.kind != BindingPlaceKind::Local {
        return env.log_error(
            inner_expr.token_range(),
            &catalogue::MOVING_OUT_OF_AGGREGATE_FIELDS_OR_PROJECTIONS_IS_NOT_IMPLEMENTED,
            (),
        );
    };

    if !matches!(inner_val.kind, THIRExpressionKind::Variable { .. }) {
        return env.log_error(
            inner_expr.token_range(),
            &catalogue::MOVE_EXPRESSIONS_CAN_CURRENTLY_ONLY_BE_APPLIED_TO_STACK_VARIABLE,
            (),
        );
    }

    let Some(inner_type) = env.symbols.mem_ref_inner(&inner_val._type).cloned() else {
        unreachable!()
    };

    if owned_unsafe_move(env, &inner_type) && env.function.in_safe_context() {
        return env.log_error(
            inner_expr.token_range(),
            &catalogue::MOVING_A_VALUE_OF_AN_UNSAFE_MOVE_TYPE_MUST_BE,
            (),
        );
    }

    Ok(TypecheckResult::new(
        inner_type,
        THIRExpressionKind::Move {
            name: binding.root.clone(),
            local_id: binding.local_id,
        },
    )
    .with_binding(binding))
}

fn owned_unsafe_move(env: &TypeEnvironment, ty: &THIRType) -> bool {
    match &ty.kind {
        THIRTypeKind::Structured { .. }
        | THIRTypeKind::Union { .. }
        | THIRTypeKind::TaggedUnion { .. } => ty.is_unsafe_move(),
        THIRTypeKind::Array { inner_type, .. } => {
            owned_unsafe_move(env, env.symbols.resolve_type_id(*inner_type))
        }
        _ => false,
    }
}

pub(crate) fn typecheck_adopt(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    inner: &HIRExpression,
) -> CXResult<TypecheckResult> {
    if env.function.in_safe_context() {
        return env.log_error(
            expr.token_range(),
            &catalogue::ADOPT_IS_UNSAFE_AND_MUST_BE_WRAPPED_IN_UNSAFE_IN,
            (),
        );
    }

    let value = typecheck_expr(env, namespace, inner, None)?;
    let binding = value.binding().cloned();
    let value = value.standard_ready_coerce(env, inner.token_range())?;
    let Some(inner_type) = env.symbols.mem_ref_inner(&value._type).cloned() else {
        return env.log_error(
            expr.token_range(),
            &catalogue::ADOPT_REQUIRES_AN_ADDRESSABLE_MEMORY_PLACE,
            (),
        );
    };

    if value._type.get_specifier(HIR_CONST) || inner_type.get_specifier(HIR_CONST) {
        return env.log_error(
            expr.token_range(),
            &catalogue::ADOPT_CANNOT_ADOPT_FROM_A_CONST_MEMORY_PLACE,
            (),
        );
    }

    if let Some(binding) = binding.as_ref()
        && binding.kind == BindingPlaceKind::Local
    {
        return env.log_error(
            expr.token_range(),
            &catalogue::ADOPT_OF_A_LOCAL_BINDING_IS_NOT_ALLOWED_USE_MOVE,
            (),
        );
    }

    Ok(
        TypecheckResult::new(inner_type, THIRExpressionKind::Typechange(Box::new(value)))
            .with_adopting(),
    )
}

pub(crate) fn typecheck_leak(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    inner: &HIRExpression,
) -> CXResult<TypecheckResult> {
    if env.function.in_safe_context() {
        return env.log_error(
            expr.token_range(),
            &catalogue::LEAK_IS_UNSAFE_AND_MUST_BE_WRAPPED_IN_UNSAFE_IN,
            (),
        );
    }

    let value = typecheck_expr(env, namespace, inner, None)?;

    let Some(binding) = value.binding().cloned() else {
        return env.log_error(
            expr.token_range(),
            &catalogue::LEAK_CURRENTLY_REQUIRES_A_LOCAL_IDENTIFIER,
            (),
        );
    };

    if binding.kind != BindingPlaceKind::Local {
        return env.log_error(
            expr.token_range(),
            &catalogue::LEAK_ON_AGGREGATE_FIELDS_OR_PROJECTIONS_IS_NOT_IMPLEMENTED,
            (),
        );
    };

    let value = value.standard_ready_coerce(env, inner.token_range())?;

    let Some(inner_type) = env.symbols.mem_ref_inner(&value._type).cloned() else {
        return env.log_error(
            expr.token_range(),
            &catalogue::LEAK_REQUIRES_A_STACK_LOCAL_VALUE,
            (),
        );
    };

    if !inner_type.is_nodrop() {
        return Ok(TypecheckResult::from(value));
    }

    Ok(TypecheckResult::new(
        THIRType::unit(),
        THIRExpressionKind::LeakLifetime {
            expression: Box::new(value),
        },
    ))
}

pub(crate) fn typecheck_unpack(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    inner: &HIRExpression,
    bindings: &[HIRUnpackBinding],
) -> CXResult<TypecheckResult> {
    let value = typecheck_expr(env, namespace, inner, None)?
        .standard_ready_assure(env, expr.token_range())?;

    let thir_expr = value.standard_ready_coerce(env, inner.token_range())?;
    let THIRTypeKind::Structured { fields } = &thir_expr._type.kind else {
        return env.log_error(
            expr.token_range(),
            &catalogue::UNPACK_EXPECTS_AN_OWNED_STRUCT_TYPE,
            (),
        );
    };

    let field_map = fields
        .iter()
        .enumerate()
        .filter_map(|(index, field)| Some((field.name()?.to_string(), (index, field.ty()))))
        .collect::<HashMap<_, _>>();

    let mut seen_fields = HashSet::new();
    let mut seen_bindings = HashSet::new();

    for unpack_binding in bindings {
        if !field_map.contains_key(unpack_binding.field.as_str()) {
            return env.log_error(
                expr.token_range(),
                &catalogue::UNPACK_FIELD_DOES_NOT_EXIST_ON,
                (
                    format!("{}", unpack_binding.field),
                    format!("{}", thir_expr._type.display_with(&env.symbols)),
                ),
            );
        }

        if !seen_fields.insert(unpack_binding.field.as_string()) {
            return env.log_error(
                expr.token_range(),
                &catalogue::UNPACK_FIELD_IS_BOUND_MORE_THAN_ONCE,
                format!("{}", unpack_binding.field),
            );
        }

        if !seen_bindings.insert(unpack_binding.binding.as_string()) {
            return env.log_error(
                expr.token_range(),
                &catalogue::UNPACK_BINDING_IS_INTRODUCED_MORE_THAN_ONCE,
                format!("{}", unpack_binding.binding),
            );
        }
    }

    for (field_name, (_, field_ty_id)) in field_map.iter() {
        let _ty = env.symbols.resolve_type_id(*field_ty_id);
        if _ty.is_nodrop() && !seen_fields.contains(field_name) {
            return env.log_error(
                expr.token_range(),
                &catalogue::UNPACK_OF_MUST_BIND_NODROP_FIELD,
                (
                    format!("{}", thir_expr._type.display_with(&env.symbols)),
                    format!("{}", field_name),
                ),
            );
        }
    }

    let mut thir_bindings = Vec::new();

    for unpack_binding in bindings {
        let local_id = THIRLocalID::fresh();

        let Some(field_pos) = fields.iter().position(|f| {
            f.name()
                .map(|n| n == unpack_binding.field.as_str())
                .unwrap_or(false)
        }) else {
            return env.log_error(
                expr.token_range(),
                &catalogue::UNPACK_FIELD_DOES_NOT_EXIST_ON,
                (
                    format!("{}", unpack_binding.field),
                    format!("{}", thir_expr._type.display_with(&env.symbols)),
                ),
            );
        };

        let field_type = env.symbols.resolve_type_id(fields[field_pos].ty()).clone();
        let symbol_type = env.symbols.mem_ref_to(field_type.clone());

        env.symbols.insert_local_value(
            QualifiedName::new_raw(unpack_binding.binding.clone()),
            THIRExpression {
                token_range: TokenRange::internal(),
                _type: symbol_type,
                kind: THIRExpressionKind::Variable {
                    name: unpack_binding.binding.clone(),
                    local_id,
                },
            },
        );

        thir_bindings.push(THIRUnpackBinding {
            field_name: unpack_binding.field.clone(),
            field_index: field_pos,
            field_type,

            binding_name: unpack_binding.binding.clone(),
            binding_local_id: local_id,
        })
    }

    Ok(TypecheckResult::new(
        THIRType::unit(),
        THIRExpressionKind::Unpack {
            value: Box::new(thir_expr),
            bindings: thir_bindings,
        },
    ))
}
