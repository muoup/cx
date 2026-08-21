use crate::environment::{ScopeExitTarget, TypeEnvironment};
use crate::type_checking::coercion::implicit::{implicit_cast, promotion::std_rval_promotion};
use crate::type_checking::control_flow::expr_may_fall_through;
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_hir::ast::expression::{HIRExprKind, HIRExpression};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::THIRExpressionKind,
};
use cx_tokens::TokenRange;

fn case_body_expression(
    block: &[HIRExpression],
    start: usize,
    end: usize,
    fallback_range: &TokenRange,
) -> HIRExpression {
    let expressions = block[start..end].to_vec();
    let range = expressions
        .first()
        .map(|expression| expression.range.clone())
        .unwrap_or_else(|| fallback_range.clone());
    HIRExpression {
        kind: HIRExprKind::Block {
            exprs: expressions,
            creates_scope: false,
        },
        range,
    }
}

fn next_case_boundary(
    block_len: usize,
    start: usize,
    cases: &[(HIRExpression, usize)],
    default_case: Option<&usize>,
) -> usize {
    cases
        .iter()
        .map(|(_, index)| *index)
        .chain(default_case.copied())
        .filter(|index| *index > start)
        .min()
        .unwrap_or(block_len)
        .min(block_len)
}

pub fn typecheck_switch(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    condition: &HIRExpression,
    block: &[HIRExpression],
    cases: &[(HIRExpression, usize)],
    default_case: Option<&usize>,
) -> CXResult<TypecheckResult> {
    env.push_scope(true, false);
    env.function.set_scope_anchor(condition);
    env.function.configure_merge_scope(condition, None);

    let join_scope_idx = env.function.current_scope_index();
    let condition_value = typecheck_expr(env, namespace, condition, None)
        .and_then(|v| v.standard_ready_coerce(env, condition.token_range()))
        .and_then(|v| std_rval_promotion(env, v))?;
    let THIRTypeKind::Integer { .. } = condition_value.get_type().kind else {
        return env.log_error(
            &condition_value.token_range,
            format!(
                "Switch condition must be an integer type, found {}",
                condition_value.get_type().display_with(&env.symbols)
            ),
        );
    };
    let condition_type = condition_value.get_type().clone();
    let base_snapshot = env.function.current_snapshot();

    let mut arms = Vec::new();

    for (case_expr, case_index) in cases {
        let case_index = *case_index;
        if case_index > block.len() {
            return env.log_error(
                &condition_value.token_range,
                format!(
                    "Switch case index {} out of bounds (block has {} expressions)",
                    case_index,
                    block.len()
                ),
            );
        }
        let case_end = next_case_boundary(block.len(), case_index, cases, default_case);
        let case_body = case_body_expression(block, case_index, case_end, case_expr.token_range());

        let case_value = typecheck_expr(env, namespace, case_expr, None)
            .and_then(|v| v.standard_ready_coerce(env, case_expr.token_range()))
            .and_then(|v| std_rval_promotion(env, v))
            .and_then(|v| implicit_cast(env, v, &condition_type))?;

        let case_body_expr = typecheck_expr(env, namespace, &case_body, None)
            .and_then(|v| v.standard_ready_coerce(env, case_body.token_range()))?;
        if expr_may_fall_through(&case_body_expr) {
            env.function.enqueue_scope_arrow(
                &ScopeExitTarget {
                    target_scope: join_scope_idx,
                    sink: crate::environment::ScopeArrowSink::Merge,
                    label: format!("case {}", case_index),
                },
                env.function.current_snapshot(),
            );
        }
        env.function.restore_snapshot(&base_snapshot);

        arms.push((Box::new(case_value), Box::new(case_body_expr)));
    }

    // Handle default case
    let default_body = match default_case {
        Some(&idx) => {
            if idx > block.len() {
                return env.log_error(
                    condition_value.token_range,
                    format!(
                        "Switch default case index {} out of bounds (block has {} expressions)",
                        idx,
                        block.len()
                    ),
                );
            }
            let end = next_case_boundary(block.len(), idx, cases, default_case);
            let expr = case_body_expression(block, idx, end, &condition_value.token_range);
            let body_expr = typecheck_expr(env, namespace, &expr, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
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
        THIRType::unit(),
        THIRExpressionKind::CSwitch {
            condition: Box::new(condition_value),
            cases: arms,
            default: default_body,
        },
    ))
}
