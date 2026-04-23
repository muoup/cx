use crate::{
    environment::{BindingMoveState, TypeEnvironment},
    log_typecheck_error,
    type_checking::{
        coercion::implicit::implicit_cast, result::TypecheckResult,
        value::locals::ensure_binding_available,
    },
};
use cx_ast::ast::{CXExpr, CXExprKind};
use cx_mir::mir::{data::MIRType, expression::MIRExpressionKind};
use cx_util::CXResult;

pub(crate) fn typecheck_move(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    inner_expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let CXExprKind::Identifier(ident) = &inner_expr.kind else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Move expressions can currently only be applied to stack variable identifiers"
        );
    };

    let Some(inner_val) = env.function.symbol_value(ident.as_str()) else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Identifier '{}' not found",
            ident
        );
    };
    let mut inner_val = inner_val.clone();

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
        ensure_binding_available(env, inner_expr, ident)?;
        env.function
            .set_tracked_binding_state(ident.as_str(), BindingMoveState::Moved);
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
    expr: &CXExpr,
    inner: &CXExpr,
) -> CXResult<TypecheckResult> {
    if env.function.in_safe_context() {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@leak is unsafe and must be wrapped in @unsafe in safe functions"
        );
    }

    let CXExprKind::Identifier(ident) = &inner.kind else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "@leak currently requires a local identifier"
        );
    };

    let Some(value) = env.function.symbol_value(ident.as_str()) else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Identifier '{}' not found",
            ident
        );
    };
    let value = value.clone();

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

    ensure_binding_available(env, inner, ident)?;
    env.function
        .set_tracked_binding_state(ident.as_str(), BindingMoveState::Moved);

    Ok(TypecheckResult::new_base(
        MIRType::unit(),
        MIRExpressionKind::LeakLifetime {
            expression: Box::new(value),
        },
    ))
}
