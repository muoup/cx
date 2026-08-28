use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind};

pub(crate) mod r#match;
pub(crate) mod r#return;
pub(crate) mod switch;
pub(crate) mod r#yield;

pub(crate) fn expr_may_fall_through(expr: &THIRExpression) -> bool {
    if expr._type.is_unreachable() {
        return false;
    }
    match &expr.kind {
        THIRExpressionKind::Return { .. }
        | THIRExpressionKind::Yield { .. }
        | THIRExpressionKind::Break { .. }
        | THIRExpressionKind::Continue { .. }
        | THIRExpressionKind::Unreachable => false,
        THIRExpressionKind::Goto { .. } => true,
        THIRExpressionKind::Label { statement, .. } => expr_may_fall_through(statement),
        THIRExpressionKind::Unsafe { expression, .. } => expr_may_fall_through(expression),
        THIRExpressionKind::StagedExpression(staged) => expr_may_fall_through(staged.expr()),
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
            function,
            arguments,
            contract,
        } => {
            !contract.noreturn
                && arguments.iter().all(|argument| match &argument.kind {
                    THIRExpressionKind::StagedExpression(staged) => {
                        expr_may_fall_through(staged.expr())
                    }
                    _ => true,
                })
                && !matches!(
                    &function.kind,
                    THIRExpressionKind::FunctionReference { name, .. }
                        if name.as_str() == "exit"
                )
        }
        _ => true,
    }
}
