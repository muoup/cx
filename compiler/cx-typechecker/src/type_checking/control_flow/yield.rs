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
            &catalogue::INVALID_CONTEXT,
            ("Yield statement".into(), "defer context".into()),
        );
    }

    let mut state = env.function.flow().yield_state();
    if state.target == ControlTarget::Staged {
        state.expected_type = state
            .expected_type
            .or(env.staging_context().yield_type().cloned());
    }
    if state.target == ControlTarget::Invalid {
        return env.log_error(
            yield_range,
            &catalogue::INVALID_CONTEXT,
            ("Yield statement".into(), "a non-yielding context".into())
        );
    }

    let (yielded_value, yield_type, has_value) = match value {
        Some(value) => {
            let result = typecheck_expr(env, namespace, value, state.expected_type.as_ref())?;

            if state.saw_empty {
                let result_type = result.standard_ready_coerce(env, value.token_range())?.ty;

                return env.log_error(
                    yield_range,
                    &catalogue::MIXED_YIELDS,
                    (
                        Some(format!("{}", result_type.display_with(&env.symbols))),
                        None
                    )
                );
            }

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
                && !env.type_eq(&expression.ty, expected_type)
            {
                return env.log_error(
                    yield_range,
                    &catalogue::MIXED_YIELDS,
                    (
                        Some(format!("{}", expression.ty.display_with(&env.symbols))),
                        Some(format!("{}", expected_type.display_with(&env.symbols))),
                    ),
                );
            }

            let yield_type = expression.ty.clone();
            (Some(Box::new(expression)), yield_type, true)
        }
        None => {
            if state.saw_value {
                return env.log_error(
                    yield_range,
                    &catalogue::MIXED_YIELDS,
                    (None, Some(state.expected_type.unwrap().display_with(&env.symbols).to_string()))
                );
            }

            if let Some(expected_type) = &state.expected_type
                && !expected_type.is_void()
            {
                return env.log_error(
                    yield_range,
                    &catalogue::MIXED_YIELDS,
                    (None, Some(expected_type.display_with(&env.symbols).to_string()))
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
