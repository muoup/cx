use crate::{
    environment::{ScopeArrowSink, ScopeExitTarget, TypeEnvironment},
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        control_flow::enqueue_jump_arrow,
        result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};
use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    thir::{data::THIRType, expression::THIRExpressionKind},
};
use cx_tokens::TokenRange;

pub fn typecheck_yield(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    yield_range: &TokenRange,
    value: Option<&HIRExpression>,
) -> CXResult<TypecheckResult> {
    if env.in_defer_context() {
        return env.log_error(
            yield_range,
            "yield is not allowed inside a deferred expression".to_string(),
        );
    }

    let Some(context) = env.function.current_yield_context().cloned() else {
        return env.log_error(
            yield_range,
            "'yield' used outside of a yielding context".to_string(),
        );
    };

    let target_type = context.result_type.clone();
    let yielded_value = match value {
        Some(value) => {
            if context.saw_empty_yield {
                return env.log_error(
                    yield_range,
                    "A yield context cannot mix value and valueless yields".to_string(),
                );
            }
            let mut expr = typecheck_expr(env, namespace, value, target_type.as_ref())?
                .standard_ready_coerce(env, value.token_range())?;
            if target_type
                .as_ref()
                .is_none_or(|ty| !ty.is_memory_reference())
            {
                expr = std_rval_promotion(env, expr)?;
            }
            let expr = if let Some(target_type) = target_type.as_ref() {
                implicit_cast(env, expr, target_type)?
            } else {
                expr
            };
            if target_type.is_none() {
                if let Some(context) = env.function.current_yield_context_mut() {
                    context.result_type = Some(expr._type.clone());
                }
            }
            Some(Box::new(expr))
        }

        None if target_type.as_ref().is_some_and(THIRType::is_unit) => {
            if let Some(context) = env.function.current_yield_context_mut() {
                context.saw_empty_yield = true;
            }
            None
        }

        None if target_type.is_some() => {
            let target_type = target_type.as_ref().expect("yield target type disappeared");
            return env.log_error(
                yield_range,
                format!(
                    "Yield target expects a value of type {}",
                    target_type.display_with(&env.symbols)
                ),
            );
        }

        None => {
            if let Some(context) = env.function.current_yield_context_mut() {
                context.saw_empty_yield = true;
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
        THIRType::unit(),
        THIRExpressionKind::Yield {
            value: yielded_value,
        },
    ))
}
