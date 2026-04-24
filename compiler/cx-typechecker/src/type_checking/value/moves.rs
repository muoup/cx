use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        coercion::implicit::implicit_cast,
        result::{BindingPlaceKind, TypecheckResult},
        typechecker::typecheck_place_expr,
        value::locals::{ensure_place_available, mark_local_place_moved},
    },
};
use cx_ast::ast::CXExpression;
use cx_mir::mir::{data::MIRType, expression::MIRExpressionKind, program::MIRBaseMappings};
use cx_util::CXResult;

pub(crate) fn typecheck_move(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    inner_expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let inner = typecheck_place_expr(env, base_data, inner_expr)?;
    
    let Some(binding) = inner.binding.clone() else {
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

    let mut inner_val = inner.into_expression();

    if !matches!(inner_val.kind, MIRExpressionKind::Variable(_)) {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Move expressions can currently only be applied to stack variable identifiers, found {:?}",
            inner_val.kind
        );
    }

    let Some(inner_type) = env.symbols.context.mem_ref_inner(&inner_val._type).cloned() else {
        unreachable!()
    };

    if env.symbols.is_nocopy(&inner_type) {
        ensure_place_available(env, Some(inner_expr.token_range().clone()), &binding)?;
        mark_local_place_moved(env, &binding);
    } else {
        inner_val = implicit_cast(env, inner_val, &inner_type)?;
    }

    Ok(TypecheckResult::new_base(
        inner_type,
        MIRExpressionKind::RegionMove {
            source: Box::new(inner_val),
        },
    ))
}

pub(crate) fn typecheck_leak(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
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

    let value = typecheck_place_expr(env, base_data, inner)?;
    let Some(binding) = value.binding.clone() else {
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

    let value = value.into_expression();

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

    ensure_place_available(env, Some(inner.token_range().clone()), &binding)?;
    mark_local_place_moved(env, &binding);

    Ok(TypecheckResult::new_base(
        MIRType::unit(),
        MIRExpressionKind::LeakLifetime {
            expression: Box::new(value),
        },
    ))
}
