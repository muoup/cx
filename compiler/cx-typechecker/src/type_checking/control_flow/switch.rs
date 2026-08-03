use crate::environment::{ScopeExitTarget, TypeEnvironment};
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::control_flow::expr_may_fall_through;
use crate::type_checking::control_flow::append_current_scope_cleanups;
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::expression::CXExpression;
use cx_log::CXResult;
use cx_mir::EnvironmentNamespace;
use cx_mir::mir::{
    data::{MIRType, MIRTypeKind},
    expression::{MIRExpression, MIRExpressionKind},
};
use cx_tokens::TokenRange;

pub fn typecheck_switch(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    condition: &CXExpression,
    block: &[CXExpression],
    cases: &[(u64, usize)],
    default_case: Option<&usize>,
) -> CXResult<TypecheckResult> {
    env.push_scope(true, false);
    env.function.set_scope_anchor(condition);
    env.function.configure_merge_scope(condition, None, false);

    let join_scope_idx = env.function.current_scope_index();
    let condition_value = typecheck_expr(env, namespace, condition, None)
        .and_then(|v| v.standard_ready_coerce(env, condition.token_range()))
        .and_then(|v| std_rval_promotion(env, v))?;
    let base_snapshot = env.function.current_snapshot();

    // Build match arms from the cases
    // Each case maps a constant value to a range of expressions in the block
    let mut arms = Vec::new();

    for (case_index, case_value) in cases {
        // Find the expression at this case index
        let Some(case_expr) = block.get(*case_index as usize) else {
            return env.log_error(
                &condition_value.token_range,
                format!(
                    "Switch case index {} out of bounds (block has {} expressions)",
                    *case_index,
                    block.len()
                ),
            );
        };

        env.push_child_defer_scope();
        let case_body_expr = typecheck_expr(env, namespace, case_expr, None)
            .and_then(|v| v.standard_ready_coerce(env, case_expr.token_range()))?;
        let case_body_expr = append_current_scope_cleanups(env, case_body_expr);
        env.pop_defer_scope();
        if expr_may_fall_through(&case_body_expr) {
            env.function.enqueue_scope_arrow(
                &ScopeExitTarget {
                    target_scope: join_scope_idx,
                    sink: crate::environment::ScopeArrowSink::Merge,
                    label: format!("case {}", case_value),
                },
                env.function.current_snapshot(),
            );
        }
        env.function.restore_snapshot(&base_snapshot);

        // Create a pattern expression that matches the constant value
        // Use the condition's integer type for the pattern
        let MIRTypeKind::Integer { _type, signed } = &condition_value.get_type().kind else {
            return env.log_error(
                &condition_value.token_range,
                format!(
                    "Switch condition must be an integer type, found {}",
                    condition_value.get_type().display_with(&env.symbols)
                ),
            );
        };

        let pattern_expr = MIRExpression {
            token_range: TokenRange::internal(),
            kind: MIRExpressionKind::IntLiteral(*case_value as i64),
            _type: MIRType::from(MIRTypeKind::Integer {
                signed: *signed,
                _type: *_type,
            }),
        };

        arms.push((Box::new(pattern_expr), Box::new(case_body_expr)));
    }

    // Handle default case
    let default_body = match default_case {
        Some(&idx) => {
            let Some(expr) = block.get(idx) else {
                return env.log_error(
                    condition_value.token_range,
                    format!(
                        "Switch default case index {} out of bounds (block has {} expressions)",
                        idx,
                        block.len()
                    ),
                );
            };
            env.push_child_defer_scope();
            let body_expr = typecheck_expr(env, namespace, expr, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
            let body_expr = append_current_scope_cleanups(env, body_expr);
            env.pop_defer_scope();
            if expr_may_fall_through(&body_expr) {
                env.function.enqueue_scope_arrow(
                    &ScopeExitTarget {
                        target_scope: join_scope_idx,
                        sink: crate::environment::ScopeArrowSink::Merge,
                        label: "default".to_string(),
                    },
                    env.function.current_snapshot(),
                );
            }
            env.function.restore_snapshot(&base_snapshot);
            Some(Box::new(body_expr))
        }
        None => None,
    };

    if default_case.is_none() {
        env.function.enqueue_scope_arrow(
            &ScopeExitTarget {
                target_scope: join_scope_idx,
                sink: crate::environment::ScopeArrowSink::Merge,
                label: "no case matched".to_string(),
            },
            env.function.current_snapshot(),
        );
    }

    env.pop_scope()
        .map_err(|err| env.complete_err(err, condition.token_range()))?;

    // Build the match expression
    Ok(TypecheckResult::new(
        MIRType::unit(),
        MIRExpressionKind::CSwitch {
            condition: Box::new(condition_value),
            cases: arms,
            default: default_body,
        },
    ))
}
