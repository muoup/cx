use crate::environment::{ScopeArrowSink, ScopeExitTarget, ScopeId, TypeEnvironment};
use crate::type_checking::typechecker::typecheck_expr;
use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind};

pub(crate) mod r#match;
pub(crate) mod r#return;
pub(crate) mod switch;
pub(crate) mod r#yield;

pub(crate) fn expr_may_fall_through(expr: &THIRExpression) -> bool {
    match &expr.kind {
        THIRExpressionKind::Return { .. }
        | THIRExpressionKind::Yield { .. }
        | THIRExpressionKind::Break
        | THIRExpressionKind::Continue
        | THIRExpressionKind::Unreachable => false,
        THIRExpressionKind::Goto { .. } => true,
        THIRExpressionKind::Label { statement, .. } => expr_may_fall_through(statement),
        THIRExpressionKind::Unsafe { expression, .. } => expr_may_fall_through(expression),
        THIRExpressionKind::Block { statements, .. } => {
            statements.last().map(expr_may_fall_through).unwrap_or(true)
        }
        THIRExpressionKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            expr_may_fall_through(then_branch)
                || else_branch
                    .as_ref()
                    .map(|branch| expr_may_fall_through(branch))
                    .unwrap_or(true)
        }
        THIRExpressionKind::CSwitch { cases, default, .. } => {
            cases
                .iter()
                .any(|(_, branch)| expr_may_fall_through(branch))
                || default
                    .as_ref()
                    .map(|branch| expr_may_fall_through(branch))
                    .unwrap_or(true)
        }
        THIRExpressionKind::Match {
            arms,
            default,
            exhaustive,
            ..
        } => {
            arms.iter().any(|(_, branch)| expr_may_fall_through(branch))
                || default
                    .as_ref()
                    .map(|branch| expr_may_fall_through(branch))
                    .unwrap_or(!exhaustive)
        }
        THIRExpressionKind::CallFunction {
            function, contract, ..
        } => {
            !contract.noreturn
                && !matches!(
                    &function.kind,
                    THIRExpressionKind::FunctionReference { name, .. }
                        if name.as_str() == "exit"
                )
        }
        _ => true,
    }
}

pub(crate) fn enqueue_jump_arrow(env: &mut TypeEnvironment, target: &ScopeExitTarget) {
    let snapshot = env.function.current_snapshot();
    env.function.enqueue_scope_arrow(target, snapshot);

    if env.function.current_scope_index() == target.target_scope {
        env.function.mark_current_scope_unreachable();
    } else {
        env.function.mark_jump_unreachable(target.target_scope);
    }
}

pub(crate) fn typecheck_fallthrough_scope(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    target_scope: ScopeId,
    sink: ScopeArrowSink,
    label: &str,
) -> CXResult<THIRExpression> {
    env.push_scope(false, false);
    env.function.set_scope_anchor(expr);
    env.function.set_scope_fallthrough_target(ScopeExitTarget {
        target_scope,
        sink,
        label: label.to_string(),
    });
    let result = typecheck_expr(env, namespace, expr, None)
        .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
    env.pop_scope()
        .map_err(|err| env.complete_err(err, expr.token_range()))?;
    Ok(result)
}

pub(crate) fn process_for_increment_arrows(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    loop_scope_idx: ScopeId,
    increment: &HIRExpression,
) -> CXResult<()> {
    let pending_arrows = env.function.take_pending_increment_arrows(loop_scope_idx);
    if pending_arrows.is_empty() {
        return Ok(());
    }

    let loop_entry_snapshot = env.function.loop_entry_snapshot(loop_scope_idx);

    for arrow in pending_arrows {
        env.function.restore_snapshot(&arrow.snapshot);
        env.function.set_scope_reachable(loop_scope_idx, true);

        let _ = typecheck_expr(env, namespace, increment, None)?;

        if env.function.is_scope_reachable(loop_scope_idx) {
            env.function.enqueue_scope_arrow(
                &ScopeExitTarget {
                    target_scope: loop_scope_idx,
                    sink: ScopeArrowSink::LoopContinue,
                    label: arrow.label,
                },
                env.function.current_snapshot(),
            );
        }
    }

    env.function.restore_snapshot(&loop_entry_snapshot);
    env.function.set_scope_reachable(loop_scope_idx, true);

    Ok(())
}
