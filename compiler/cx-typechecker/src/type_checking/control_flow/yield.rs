use crate::{
    environment::{ControlTarget, TypeEnvironment},
    type_checking::{
        coercion::implicit::promotion::std_rval_promotion,
        result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};
use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_thir::{
    NamespacePath,
    thir::{data::THIRType, expression::THIRExpressionKind},
};
use cx_tokens::TokenRange;

pub fn typecheck_yield(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    yield_range: &TokenRange,
    value: Option<&HIRExpression>,
) -> CXResult<TypecheckResult> {
    if env.in_defer_context() {
        return env.log_error(
            yield_range,
            "yield is not allowed inside a deferred expression".to_string(),
        );
    }

    let state = env.function.flow().yield_state();
    if state.target == ControlTarget::Invalid {
        return env.log_error(
            yield_range,
            "'yield' used outside of a yielding context".to_string(),
        );
    }

    let (yielded_value, yield_type, has_value) = match value {
        Some(value) => {
            if state.saw_empty {
                return env.log_error(
                    yield_range,
                    "A yield context cannot mix value and valueless yields".to_string(),
                );
            }

            let mut expression = typecheck_expr(
                env,
                namespace,
                value,
                state.expected_type.as_ref(),
            )?
            .standard_ready_coerce(env, value.token_range())?;
            if state
                .expected_type
                .as_ref()
                .is_none_or(|ty| !ty.is_memory_reference())
            {
                expression = std_rval_promotion(env, expression)?;
            }
            if let Some(expected_type) = &state.expected_type
                && !env.type_eq(&expression._type, expected_type)
            {
                return env.log_error(
                    yield_range,
                    format!(
                        "Yield type {} does not match {}",
                        expression._type.display_with(&env.symbols),
                        expected_type.display_with(&env.symbols),
                    ),
                );
            }

            let yield_type = expression._type.clone();
            (Some(Box::new(expression)), yield_type, true)
        }
        None => {
            if state.saw_value {
                return env.log_error(
                    yield_range,
                    "A yield context cannot mix value and valueless yields".to_string(),
                );
            }
            if let Some(expected_type) = &state.expected_type
                && !expected_type.is_void()
            {
                return env.log_error(
                    yield_range,
                    format!(
                        "Yield target expects a value of type {}",
                        expected_type.display_with(&env.symbols)
                    ),
                );
            }
            (None, THIRType::unit(), false)
        }
    };

    env.function
        .flow_mut()
        .record_yield(yield_type, has_value);

    Ok(TypecheckResult::new(
        THIRType::unit(),
        THIRExpressionKind::Yield {
            value: yielded_value,
            staged: state.target == ControlTarget::Staged,
        },
    ))
}
