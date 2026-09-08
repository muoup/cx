use crate::{
    environment::{ControlTarget, TypeEnvironment},
    type_checking::{
        coercion::implicit::promotion::std_rval_promotion, result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};
use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::NamespacePath;
use cx_thir::thir::{data::THIRType, expression::THIRExpressionKind};
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
            &catalogue::YIELD_IS_NOT_ALLOWED_INSIDE_A_DEFERRED_EXPRESSION,
            (),
        );
    }

    let mut state = env.function.flow().yield_state();
    if state.target == ControlTarget::Staged {
        state.expected_type = state.expected_type.or(env.staging_context().yield_type);
    }
    if state.target == ControlTarget::Invalid {
        return env.log_error(
            yield_range,
            &catalogue::YIELD_USED_OUTSIDE_OF_A_YIELDING_CONTEXT,
            (),
        );
    }

    let (yielded_value, yield_type, has_value) = match value {
        Some(value) => {
            if state.saw_empty {
                return env.log_error(
                    yield_range,
                    &catalogue::A_YIELD_CONTEXT_CANNOT_MIX_VALUE_AND_VALUELESS_YIELDS,
                    (),
                );
            }

            let result = typecheck_expr(env, namespace, value, state.expected_type.as_ref())?;
            let result = if let Some(expected_type) = &state.expected_type {
                result.apply_expected_type(env, namespace, expected_type)?
            } else {
                result
            };
            let mut expression = result.standard_ready_coerce(env, value.token_range())?;
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
                    &catalogue::YIELD_TYPE_DOES_NOT_MATCH,
                    (
                        format!("{}", expression._type.display_with(&env.symbols)),
                        format!("{}", expected_type.display_with(&env.symbols)),
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
                    &catalogue::A_YIELD_CONTEXT_CANNOT_MIX_VALUE_AND_VALUELESS_YIELDS,
                    (),
                );
            }
            if let Some(expected_type) = &state.expected_type
                && !expected_type.is_void()
            {
                return env.log_error(
                    yield_range,
                    &catalogue::YIELD_TARGET_EXPECTS_A_VALUE_OF_TYPE,
                    format!("{}", expected_type.display_with(&env.symbols)),
                );
            }
            (None, THIRType::unit(), false)
        }
    };

    env.function.flow_mut().record_yield(yield_type, has_value);

    Ok(TypecheckResult::new(
        THIRType::unit(),
        THIRExpressionKind::Yield {
            value: yielded_value,
        },
    ))
}
