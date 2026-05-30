use std::collections::{HashMap, HashSet};

use crate::{
    environment::{BindingMoveState, TypeEnvironment, symbols::SymbolValueOrigin},
    log_typecheck_error,
    type_checking::{
        coercion::implicit::conversion::try_argument_conversion,
        result::{BindingPlaceKind, TypecheckResult},
        typechecker::typecheck_expr,
        value::locals::{ensure_binding_available, mark_binding},
    },
};
use cx_ast::ast::{expression::{CXExpression, CXUnpackBinding}, modifiers::CX_CONST};
use cx_mir::mir::{
    data::{MIRType, MIRTypeKind},
    expression::{MIRExpression, MIRExpressionKind},
    program::EnvironmentNamespace,
};
use cx_util::{CXResult, identifier::CXIdent};

pub(crate) fn typecheck_move(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    inner_expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let inner = typecheck_expr(env, namespace, inner_expr, None)?;

    let Some(binding) = inner.binding().cloned() else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Move expressions can currently only be applied to stack variable identifiers"
        );
    };

    if binding.kind != BindingPlaceKind::Local {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Moving out of aggregate fields or projections is not implemented"
        );
    };

    let mut inner_val = inner.into_expression()?;

    if !matches!(inner_val.kind, MIRExpressionKind::Variable(_)) {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Move expressions can currently only be applied to stack variable identifiers",
        );
    }

    let Some(inner_type) = env.symbols.context.mem_ref_inner(&inner_val._type).cloned() else {
        unreachable!()
    };

    if env.symbols.is_nocopy(&inner_type) {
        ensure_binding_available(env, Some(inner_expr.token_range().clone()), &binding.root)?;
        mark_binding(env, &binding, BindingMoveState::Moved);
    } else {
        inner_val = try_argument_conversion(env, inner_val, &inner_type)?;
    }

    Ok(TypecheckResult::new_base(
        inner_type,
        MIRExpressionKind::RegionMove {
            source: Box::new(inner_val),
        },
    ))
}

pub(crate) fn typecheck_adopt(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    inner: &CXExpression,
) -> CXResult<TypecheckResult> {
    if env.function.in_safe_context() {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@adopt is unsafe and must be wrapped in @unsafe in safe functions"
        );
    }

    let value = typecheck_expr(env, namespace, inner, None)?;
    let binding = value.binding().cloned();
    let value = value.into_expression()?;
    let Some(inner_type) = env.symbols.context.mem_ref_inner(&value._type).cloned() else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@adopt requires an addressable memory place"
        );
    };

    if value._type.get_specifier(CX_CONST) || inner_type.get_specifier(CX_CONST) {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@adopt cannot adopt from a const memory place"
        );
    }

    if let Some(binding) = binding.as_ref()
        && binding.kind == BindingPlaceKind::Local
    {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@adopt of a local binding is not allowed; use move for local bindings"
        );
    }

    Ok(TypecheckResult::new_base(
        inner_type,
        MIRExpressionKind::RegionMove {
            source: Box::new(value),
        },
    )
    .with_adopting())
}

pub(crate) fn typecheck_leak(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    inner: &CXExpression,
) -> CXResult<TypecheckResult> {
    if env.function.in_safe_context() {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@leak is unsafe and must be wrapped in @unsafe in safe functions"
        );
    }

    let value = typecheck_expr(env, namespace, inner, None)?;

    let Some(binding) = value.binding().cloned() else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@leak currently requires a local identifier"
        );
    };

    if binding.kind != BindingPlaceKind::Local {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@leak on aggregate fields or projections is not implemented"
        );
    };

    let value = value.into_expression()?;

    let Some(inner_type) = env.symbols.context.mem_ref_inner(&value._type).cloned() else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@leak requires a stack local value"
        );
    };

    if !env.symbols.is_nodrop(&inner_type) {
        return Ok(TypecheckResult::from(value));
    }

    ensure_binding_available(env, Some(inner.token_range().clone()), &binding.root)?;
    mark_binding(env, &binding, BindingMoveState::Moved);

    Ok(TypecheckResult::new_base(
        MIRType::unit(),
        MIRExpressionKind::LeakLifetime {
            expression: Box::new(value),
        },
    ))
}

pub(crate) fn typecheck_unpack(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    inner: &CXExpression,
    bindings: &[CXUnpackBinding],
) -> CXResult<TypecheckResult> {
    let value = typecheck_expr(env, namespace, inner, None)?;

    let Some(source_binding) = value.binding().cloned() else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@unpack currently requires a local identifier"
        );
    };

    if source_binding.kind != BindingPlaceKind::Local {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@unpack on aggregate fields or projections is not implemented"
        );
    };

    let value = value.into_expression()?;

    let Some(inner_type) = env.symbols.context.mem_ref_inner(&value._type).cloned() else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@unpack requires a stack local value"
        );
    };

    if !matches!(inner_type.kind, MIRTypeKind::Structured { .. }) {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@unpack requires a struct type, found {}",
            inner_type.display_with(&env.symbols.context)
        );
    }

    let Some(fields) = env.symbols.context.aggregate_fields(&inner_type).cloned() else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@unpack requires a complete struct type"
        );
    };

    let mut field_map = HashMap::new();
    for (index, field) in fields.iter().enumerate() {
        let Some(name) = field.name() else {
            continue;
        };
        let Some(field_type) = env.symbols.context.get(field.type_id()).cloned() else {
            return log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "Internal error: @unpack field '{}' has an unknown type",
                name
            );
        };
        field_map.insert(name.to_string(), (index, field_type));
    }

    let mut seen_fields = HashSet::new();
    let mut seen_bindings = HashSet::new();
    for unpack_binding in bindings {
        if !field_map.contains_key(unpack_binding.field.as_str()) {
            return log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "@unpack field '{}' does not exist on {}",
                unpack_binding.field,
                inner_type.display_with(&env.symbols.context)
            );
        }

        if !seen_fields.insert(unpack_binding.field.as_string()) {
            return log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "@unpack field '{}' is bound more than once",
                unpack_binding.field
            );
        }

        if !seen_bindings.insert(unpack_binding.binding.as_string()) {
            return log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "@unpack binding '{}' is introduced more than once",
                unpack_binding.binding
            );
        }
    }

    for (field_name, (_, field_type)) in field_map.iter() {
        if env.symbols.is_nodrop(field_type) && !seen_fields.contains(field_name) {
            return log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "@unpack of {} must bind @nodrop field '{}'",
                inner_type.display_with(&env.symbols.context),
                field_name
            );
        }
    }

    ensure_binding_available(env, Some(inner.token_range().clone()), &source_binding.root)?;
    mark_binding(env, &source_binding, BindingMoveState::Moved);

    let mut statements = Vec::new();
    for unpack_binding in bindings {
        let (member_index, field_type) = field_map
            .get(unpack_binding.field.as_str())
            .expect("@unpack field existence checked above");

        let field_place = MIRExpression {
            token_range: None,
            _type: env.symbols.context.mem_ref_to(field_type.clone()),
            kind: MIRExpressionKind::MemberAccess {
                base: Box::new(value.clone()),
                member_index: *member_index,
                aggregate_type: inner_type.clone(),
            },
        };

        let initial_value = if env.symbols.is_nocopy(field_type) {
            MIRExpression {
                token_range: None,
                _type: field_type.clone(),
                kind: MIRExpressionKind::RegionMove {
                    source: Box::new(field_place),
                },
            }
        } else {
            MIRExpression {
                token_range: None,
                _type: field_type.clone(),
                kind: MIRExpressionKind::RegionDuplicate {
                    source: Box::new(field_place),
                },
            }
        };

        let binding_name = CXIdent::new(unpack_binding.binding.as_str());
        let binding_ref_type = env.symbols.context.mem_ref_to(field_type.clone());
        env.symbols.insert_value(
            binding_name.clone(),
            MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Variable(binding_name.clone()),
                _type: binding_ref_type,
            },
            Some(SymbolValueOrigin::Local),
        );
        if env.symbols.is_nocopy(field_type) {
            env.function
                .track_binding(binding_name.as_string(), env.symbols.is_nodrop(field_type));
        }

        statements.push(MIRExpression {
            token_range: None,
            _type: env.symbols.context.mem_ref_to(field_type.clone()),
            kind: MIRExpressionKind::BindRegion {
                name: binding_name,
                _type: field_type.clone(),
                initial_region: Box::new(MIRExpression {
                    token_range: None,
                    _type: env.symbols.context.mem_ref_to(field_type.clone()),
                    kind: MIRExpressionKind::RegionCreate {
                        _type: field_type.clone(),
                        initial_value: Some(Box::new(initial_value)),
                    },
                }),
                adopting: false,
            },
        });
    }

    Ok(TypecheckResult::new_base(
        MIRType::unit(),
        MIRExpressionKind::Block { statements },
    ))
}
