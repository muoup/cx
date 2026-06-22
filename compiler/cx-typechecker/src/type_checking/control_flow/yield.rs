use crate::{
    environment::{ScopeArrowSink, ScopeExitTarget, TypeEnvironment},
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        control_flow::enqueue_jump_arrow,
        result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};
use cx_ast::ast::expression::CXExpression;
use cx_log::CXResult;
use cx_mir::{
    EnvironmentNamespace,
    mir::{data::MIRType, expression::MIRExpressionKind},
};
use cx_tokens::TokenRange;

pub fn typecheck_yield(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    yield_range: &TokenRange,
    value: Option<&CXExpression>,
) -> CXResult<TypecheckResult> {
    let Some(context) = env.function.current_yield_context().cloned() else {
        return env.log_error(
            yield_range,
            "'yield' used outside of a yielding context".to_string(),
        );
    };

    let target_type = context.result_type.clone();
    let yielded_value = match (value, target_type.as_ref()) {
        (Some(value), Some(target_type)) => {
            let mut expr = typecheck_expr(env, namespace, value, Some(target_type))?
                .standard_ready_coerce(env, value.token_range())?;
            if !target_type.is_memory_reference() {
                expr = std_rval_promotion(env, expr)?;
            }
            Some(Box::new(implicit_cast(env, expr, target_type)?))
        }

        (Some(value), None) => {
            let expr = typecheck_expr(env, namespace, value, None)?
                .standard_ready_coerce(env, value.token_range())
                .and_then(|expr| std_rval_promotion(env, expr))?;
            if let Some(context) = env.function.current_yield_context_mut() {
                context.result_type = Some(expr._type.clone());
            }
            Some(Box::new(expr))
        }

        (None, Some(target_type)) if target_type.is_unit() => None,

        (None, Some(target_type)) => {
            return env.log_error(
                yield_range,
                format!(
                    "Yield target expects a value of type {}",
                    target_type.display_with(&env.symbols)
                ),
            );
        }

        (None, None) => {
            if let Some(context) = env.function.current_yield_context_mut() {
                context.result_type = Some(MIRType::unit());
            }
            None
        }
    };

    let Some(context) = env.function.current_yield_context_mut() else {
        unreachable!("Yield context disappeared while typechecking yield");
    };
    context.yield_count += 1;
    let target_scope = context.target_scope;

    enqueue_jump_arrow(
        env,
        &ScopeExitTarget {
            target_scope,
            sink: ScopeArrowSink::Merge,
            label: "yield".to_string(),
        },
    );

    Ok(TypecheckResult::new(
        MIRType::unit(),
        MIRExpressionKind::Yield {
            value: yielded_value,
            target_scope: target_scope.index(),
        },
    ))
}
