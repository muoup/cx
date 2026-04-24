use crate::environment::{ScopeExitTarget, TypeEnvironment};
use crate::log_typecheck_error;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::control_flow::expr_may_fall_through;
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::CXExpression;
use cx_mir::mir::{
    data::{MIRType, MIRTypeKind},
    expression::{MIRExpression, MIRExpressionKind},
    program::MIRBaseMappings,
};
use cx_util::CXResult;

pub fn typecheck_switch(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    condition: &CXExpression,
    block: &[CXExpression],
    cases: &[(u64, usize)],
    default_case: Option<&usize>,
) -> CXResult<TypecheckResult> {
    env.function.push_scope(true, false);
    env.function.set_scope_anchor(condition);
    env.function
        .configure_merge_scope(condition, "switch join", None, false);

    let join_scope_idx = env.function.current_scope_index();
    let condition_value = typecheck_expr(env, base_data, condition, None)
        .and_then(|val| std_rval_promotion(env, val.into_expression()))?;
    let base_snapshot = env.function.current_snapshot();

    // Build match arms from the cases
    // Each case maps a constant value to a range of expressions in the block
    let mut arms = Vec::new();

    for (case_index, case_value) in cases {
        // Find the expression at this case index
        let Some(case_expr) = block.get(*case_index as usize) else {
            return log_typecheck_error!(
                env,
                condition_value.token_range.as_ref(),
                "Switch case index {} out of bounds (block has {} expressions)",
                *case_index,
                block.len()
            );
        };

        let case_body_expr = typecheck_expr(env, base_data, case_expr, None)?.into_expression();
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
            return log_typecheck_error!(
                env,
                condition_value.token_range.as_ref(),
                "Switch condition must be an integer type, found {}",
                condition_value
                    .get_type()
                    .display_with(&env.symbols.context)
            );
        };

        let pattern_expr = MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::IntLiteral(*case_value as i64, *_type, *signed),
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
                return log_typecheck_error!(
                    env,
                    condition_value.token_range.as_ref(),
                    "Switch default case index {} out of bounds (block has {} expressions)",
                    idx,
                    block.len()
                );
            };
            let body_expr = typecheck_expr(env, base_data, expr, None)?.into_expression();
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

    env.function
        .pop_scope(env.source.compilation_unit.as_path())?;

    // Build the match expression
    Ok(TypecheckResult::new_base(
        MIRType::unit(),
        MIRExpressionKind::CSwitch {
            condition: Box::new(condition_value),
            cases: arms,
            default: default_body,
        },
    ))
}
