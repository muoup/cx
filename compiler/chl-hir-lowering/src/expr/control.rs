use chl_hir::ast::{
    expression::{HIRExprKind, HIRExpression},
    pattern::HIRPattern,
};
use chl_hmir::{
    HMIRCoerceMode,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRExprTypeKind,
        intrinsic::{HMIRIntType, HMIRIntWidth},
    },
};
use chl_log::{CHLResult, catalogue::typecheck};
use chl_tokens::TokenRange;

use crate::{
    body::{BodyLowering, ScopeKind},
    expr::{lower_expr, pattern::lower_pattern},
    log::log_hir_error,
};

pub(crate) fn lower_if<'a>(
    context: &mut BodyLowering<'a>,
    condition: &'a HIRExpression,
    then_branch: &'a HIRExpression,
    else_branch: Option<&'a HIRExpression>,
    statement: bool,
    span: &TokenRange,
) -> CHLResult<HMIRExprID> {
    let (condition, then_branch) = context.scoped(|this| -> CHLResult<_> {
        let condition = lower_condition(this, condition)?;
        let then_branch = this.scoped(|this| lower_branch(this, then_branch, statement))?;

        Ok((condition, then_branch))
    })?;
    let else_branch = else_branch
        .map(|branch| context.scoped(|this| lower_branch(this, branch, statement)))
        .transpose()?;

    Ok(context.push(
        HMIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        },
        span,
    ))
}

fn lower_branch<'a>(
    context: &mut BodyLowering<'a>,
    branch: &'a HIRExpression,
    statement: bool,
) -> CHLResult<HMIRExprID> {
    let lowered = lower_expr(context, branch)?;
    if !statement || matches!(branch.kind, HIRExprKind::Block { .. }) {
        return Ok(lowered);
    }
    Ok(context.block(vec![lowered], true, false, &branch.range))
}

pub(crate) fn lower_while<'a>(
    context: &mut BodyLowering<'a>,
    condition: &'a HIRExpression,
    body: &'a HIRExpression,
    pre_eval: bool,
    span: &TokenRange,
) -> CHLResult<HMIRExprID> {
    context.scoped(|context| {
        let condition = lower_condition(context, condition)?;
        let body = context.scoped(|context| lower_expr(context, body))?;
        Ok(context.push(
            HMIRExprKind::While {
                condition,
                body,
                pre_eval,
            },
            span,
        ))
    })
}

pub(crate) fn lower_for<'a>(
    context: &mut BodyLowering<'a>,
    init: &'a HIRExpression,
    condition: Option<&'a HIRExpression>,
    increment: &'a HIRExpression,
    body: &'a HIRExpression,
    span: &TokenRange,
) -> CHLResult<HMIRExprID> {
    context.scoped(|this| {
        let init = lower_expr(this, init)?;
        let condition = match condition {
            Some(condition) => lower_condition(this, condition)?,
            None => this.int_literal(HMIRIntType::new(HMIRIntWidth::I1, false), 1, span),
        };
        let increment = lower_expr(this, increment)?;
        let body = this.scoped(|this| lower_expr(this, body))?;

        Ok(this.push(
            HMIRExprKind::For {
                init,
                condition,
                increment,
                body,
            },
            span,
        ))
    })
}

pub(crate) fn lower_match<'a>(
    context: &mut BodyLowering<'a>,
    condition: &'a HIRExpression,
    arms: &'a [(HIRPattern, HIRExpression)],
    span: &TokenRange,
) -> CHLResult<HMIRExprID> {
    let scrutinee = lower_expr(context, condition)?;
    let subject_ty = context.hole(span);
    let subject = context.declare_local(None, subject_ty, span);
    let arms = arms
        .iter()
        .map(|(pattern, body)| {
            context.scoped(|this| {
                let pattern = lower_pattern(this, pattern)?;
                Ok((pattern, lower_expr(this, body)?))
            })
        })
        .collect::<CHLResult<_>>()?;

    Ok(context.push(
        HMIRExprKind::Match {
            scrutinee,
            subject,
            arms,
        },
        span,
    ))
}

pub(crate) fn lower_switch<'a>(
    context: &mut BodyLowering<'a>,
    condition: &'a HIRExpression,
    body: &'a HIRExpression,
    span: &TokenRange,
) -> CHLResult<HMIRExprID> {
    let condition = lower_expr(context, condition)?;
    let scope = ScopeKind::Switch { has_default: false };
    let body = context.scoped_as(scope, |this| lower_expr(this, body))?;

    Ok(context.push(HMIRExprKind::Switch { condition, body }, span))
}

pub(crate) fn lower_case<'a>(
    context: &mut BodyLowering<'a>,
    value: Option<&'a HIRExpression>,
    statement: &'a HIRExpression,
    span: &TokenRange,
) -> CHLResult<HMIRExprID> {
    if !context.in_switch() {
        return log_hir_error(
            span,
            &typecheck::REQUIRED_CONTEXT,
            ("case label".into(), "a switch".into()),
        );
    }
    let value = match value {
        Some(value) => Some(lower_expr(context, value)?),
        None if context.declare_switch_default() => None,
        None => {
            return log_hir_error(
                span,
                &typecheck::DUPLICATE_ITEM,
                ("default label".into(), "switch statement".into()),
            );
        }
    };
    let body = lower_expr(context, statement)?;

    Ok(context.push(HMIRExprKind::Case { value, body }, span))
}

fn lower_condition<'a>(
    context: &mut BodyLowering<'a>,
    condition: &'a HIRExpression,
) -> CHLResult<HMIRExprID> {
    let span = &condition.range;
    let bool_ty = HMIRIntType::new(HMIRIntWidth::I1, false);
    let value = lower_expr(context, condition)?;
    let target = context.type_ref(HMIRExprTypeKind::Int(bool_ty), span);

    Ok(context.coerce(HMIRCoerceMode::Truthy, value, target, span))
}
