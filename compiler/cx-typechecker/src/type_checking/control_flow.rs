use cx_thir::thir::expression::{THIRBlockKind, THIRExpression, THIRExpressionKind};

pub(crate) mod r#match;
pub(crate) mod r#return;
pub(crate) mod switch;
pub(crate) mod ternary;
pub(crate) mod r#yield;

/// Searches a switch body for a statement that `found` accepts, without entering the loops and
/// switches that would take a `break` or a `default` for themselves.
fn switch_body_has(expr: &THIRExpression, found: &impl Fn(&THIRExpressionKind) -> bool) -> bool {
    if found(&expr.kind) {
        return true;
    }
    match &expr.kind {
        THIRExpressionKind::Block { statements, .. } => {
            statements.iter().any(|s| switch_body_has(s, found))
        }
        THIRExpressionKind::Label { statement, .. } | THIRExpressionKind::Case { statement, .. } => {
            switch_body_has(statement, found)
        }
        THIRExpressionKind::Unsafe { expression, .. } => switch_body_has(expression, found),
        THIRExpressionKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            switch_body_has(then_branch, found)
                || else_branch
                    .as_ref()
                    .is_some_and(|branch| switch_body_has(branch, found))
        }
        THIRExpressionKind::Match { arms, .. } => {
            arms.iter().any(|(_, arm)| switch_body_has(arm, found))
        }
        _ => false,
    }
}

pub(crate) fn expr_may_fall_through(expr: &THIRExpression) -> bool {
    if expr.ty.is_unreachable() {
        return false;
    }
    match &expr.kind {
        THIRExpressionKind::Return { .. }
        | THIRExpressionKind::Yield { .. }
        | THIRExpressionKind::Break
        | THIRExpressionKind::Continue
        | THIRExpressionKind::Unreachable => false,
        THIRExpressionKind::Goto { .. } => true,
        THIRExpressionKind::Label { statement, .. } => expr_may_fall_through(statement),
        THIRExpressionKind::Unsafe { expression, .. } => expr_may_fall_through(expression),
        THIRExpressionKind::Block {
            kind: THIRBlockKind::Expression,
            yields: true,
            ..
        } => true,
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
        THIRExpressionKind::Case { statement, .. } => expr_may_fall_through(statement),
        THIRExpressionKind::CSwitch { body, .. } => {
            let has_default = switch_body_has(body, &|kind| {
                matches!(kind, THIRExpressionKind::Case { value: None, .. })
            });
            let has_break = switch_body_has(body, &|kind| matches!(kind, THIRExpressionKind::Break));
            !has_default || has_break || expr_may_fall_through(body)
        }
        THIRExpressionKind::Match { .. } if !expr.ty.is_void() => true,
        THIRExpressionKind::Match { arms, .. } => {
            arms.iter().any(|(_, branch)| expr_may_fall_through(branch))
        }
        _ => true,
    }
}
