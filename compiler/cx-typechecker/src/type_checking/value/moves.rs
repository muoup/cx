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
use cx_thir::{
    EnvironmentNamespace,
    thir::{
        data::{THIRType, THIRTypeKind},
        expression::{SymbolValueOrigin, THIRExpression, THIRExpressionKind, THIRLocalID},
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

pub(crate) fn typecheck_move(
    env: &mut TypeEnvironment,
    _: &EnvironmentNamespace,
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
            "Move expressions can currently only be applied to stack variable identifiers"
                .to_string(),
        );
    };

    if binding.kind != BindingPlaceKind::Local {
        return env.log_error(
            inner_expr.token_range(),
            "Moving out of aggregate fields or projections is not implemented".to_string(),
        );
    };

    if !matches!(inner_val.kind, THIRExpressionKind::Variable { .. }) {
        return env.log_error(
            inner_expr.token_range(),
            "Move expressions can currently only be applied to stack variable identifiers"
                .to_string(),
        );
    }

    let Some(inner_type) = env.symbols.mem_ref_inner(&inner_val._type).cloned() else {
        unreachable!()
    };

    if owned_unsafe_move(env, &inner_type) && env.function.in_safe_context() {
        return env.log_error(
            inner_expr.token_range(),
            "Moving a value of an @unsafe_move type must be wrapped in @unsafe in safe functions"
                .to_string(),
        );
    }

    Ok(TypecheckResult::new(
        inner_type,
        THIRExpressionKind::RegionMove {
            source: Box::new(inner_val),
        },
    ))
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
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    inner: &HIRExpression,
) -> CXResult<TypecheckResult> {
    if env.function.in_safe_context() {
        return env.log_error(
            expr.token_range(),
            "@adopt is unsafe and must be wrapped in @unsafe in safe functions".to_string(),
        );
    }

    let value = typecheck_expr(env, namespace, inner, None)?;
    let binding = value.binding().cloned();
    let value = value.standard_ready_coerce(env, inner.token_range())?;
    let Some(inner_type) = env.symbols.mem_ref_inner(&value._type).cloned() else {
        return env.log_error(
            expr.token_range(),
            "@adopt requires an addressable memory place".to_string(),
        );
    };

    if value._type.get_specifier(HIR_CONST) || inner_type.get_specifier(HIR_CONST) {
        return env.log_error(
            expr.token_range(),
            "@adopt cannot adopt from a const memory place".to_string(),
        );
    }

    if let Some(binding) = binding.as_ref()
        && binding.kind == BindingPlaceKind::Local
    {
        return env.log_error(
            expr.token_range(),
            "@adopt of a local binding is not allowed; use move for local bindings".to_string(),
        );
    }

    Ok(
        TypecheckResult::new(inner_type, THIRExpressionKind::Typechange(Box::new(value)))
            .with_adopting(),
    )
}

pub(crate) fn typecheck_leak(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    inner: &HIRExpression,
) -> CXResult<TypecheckResult> {
    if env.function.in_safe_context() {
        return env.log_error(
            expr.token_range(),
            "@leak is unsafe and must be wrapped in @unsafe in safe functions".to_string(),
        );
    }

    let value = typecheck_expr(env, namespace, inner, None)?;

    let Some(binding) = value.binding().cloned() else {
        return env.log_error(
            expr.token_range(),
            "@leak currently requires a local identifier".to_string(),
        );
    };

    if binding.kind != BindingPlaceKind::Local {
        return env.log_error(
            expr.token_range(),
            "@leak on aggregate fields or projections is not implemented".to_string(),
        );
    };

    let value = value.standard_ready_coerce(env, inner.token_range())?;

    let Some(inner_type) = env.symbols.mem_ref_inner(&value._type).cloned() else {
        return env.log_error(
            expr.token_range(),
            "@leak requires a stack local value".to_string(),
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
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    inner: &HIRExpression,
    bindings: &[HIRUnpackBinding],
) -> CXResult<TypecheckResult> {
    let value = typecheck_expr(env, namespace, inner, None)
        .and_then(|v| v.standard_ready_coerce(env, inner.token_range()))?;

    let THIRTypeKind::Structured { fields } = &value._type.kind else {
        return env.log_error(
            expr.token_range(),
            "@unpack expects a struct type".to_string(),
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
                format!(
                    "@unpack field '{}' does not exist on {}",
                    unpack_binding.field,
                    value._type.display_with(&env.symbols)
                ),
            );
        }

        if !seen_fields.insert(unpack_binding.field.as_string()) {
            return env.log_error(
                expr.token_range(),
                format!(
                    "@unpack field '{}' is bound more than once",
                    unpack_binding.field
                ),
            );
        }

        if !seen_bindings.insert(unpack_binding.binding.as_string()) {
            return env.log_error(
                expr.token_range(),
                format!(
                    "@unpack binding '{}' is introduced more than once",
                    unpack_binding.binding
                ),
            );
        }
    }

    for (field_name, (_, field_ty_id)) in field_map.iter() {
        let _ty = env.symbols.resolve_type_id(*field_ty_id);

        if _ty.is_nodrop() && !seen_fields.contains(field_name) {
            return env.log_error(
                expr.token_range(),
                format!(
                    "@unpack of {} must bind @nodrop field '{}'",
                    value._type.display_with(&env.symbols),
                    field_name
                ),
            );
        }
    }

    let mut statements = Vec::new();
    for unpack_binding in bindings {
        let (member_index, field_ty_id) = field_map
            .get(unpack_binding.field.as_str())
            .expect("@unpack field existence checked above");

        let field_type = env.symbols.resolve_type_id(*field_ty_id).clone();

        let field_place = THIRExpression {
            token_range: TokenRange::internal(),
            _type: env.symbols.mem_ref_to(field_type.clone()),
            kind: THIRExpressionKind::MemberAccess {
                base: Box::new(value.clone()),
                member_index: *member_index,
                aggregate_type: value._type.clone(),
            },
        };

        let initial_value = THIRExpression {
            token_range: TokenRange::internal(),
            _type: field_type.clone(),
            kind: THIRExpressionKind::RegionMove {
                source: Box::new(field_place),
            },
        };

        let binding_name = CXIdent::new(unpack_binding.binding.as_str());
        let local_id = THIRLocalID::fresh();
        let binding_ref_type = env.symbols.mem_ref_to(field_type.clone());
        env.symbols.insert_local_value(
            QualifiedName::new_raw(binding_name.clone()),
            THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Variable {
                    name: binding_name.clone(),
                    local_id: Some(local_id),
                    location: SymbolValueOrigin::Local,
                },
                _type: binding_ref_type,
            },
        );
        statements.push(THIRExpression {
            token_range: TokenRange::internal(),
            _type: env.symbols.mem_ref_to(field_type.clone()),
            kind: THIRExpressionKind::BindRegion {
                name: binding_name,
                local_id,
                _type: field_type.clone(),
                initial_region: Box::new(THIRExpression {
                    token_range: TokenRange::internal(),
                    _type: env.symbols.mem_ref_to(field_type.clone()),
                    kind: THIRExpressionKind::RegionCreate {
                        _type: field_type.clone(),
                        initial_value: Some(Box::new(initial_value)),
                    },
                }),
                adopting: false,
            },
        });
    }

    Ok(TypecheckResult::new(
        THIRType::unit(),
        THIRExpressionKind::Block {
            statements,
            creates_scope: false,
        },
    ))
}
