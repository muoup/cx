use cx_hir::ast::{
    expression::{HIRExprKind, HIRExpression},
    pattern::HIRPattern,
};
use cx_hmir::{
    HMIRCoerceMode,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRExprTypeKind,
        intrinsic::{HMIRIntType, HMIRIntWidth},
    },
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    body::{BodyLowering, ScopeKind},
    expr::{lower_expr, pattern::lower_pattern},
    log::log_hir_error,
};

pub(crate) fn lower_if(
    context: &mut BodyLowering<'_>,
    condition: &HIRExpression,
    then_branch: &HIRExpression,
    else_branch: Option<&HIRExpression>,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let (condition, then_branch) = context.scoped(|this| -> CXResult<_> {
        let condition = lower_condition(this, condition)?;
        let then_branch = this.scoped(|this| lower_expr(this, then_branch))?;

        Ok((condition, then_branch))
    })?;
    let else_branch = else_branch
        .map(|branch| context.scoped(|this| lower_expr(this, branch)))
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

pub(crate) fn lower_while(
    context: &mut BodyLowering<'_>,
    condition: &HIRExpression,
    body: &HIRExpression,
    pre_eval: bool,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
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

pub(crate) fn lower_for(
    context: &mut BodyLowering<'_>,
    init: &HIRExpression,
    condition: &HIRExpression,
    increment: &HIRExpression,
    body: &HIRExpression,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    context.scoped(|this| {
        let init = lower_expr(this, init)?;
        let condition = lower_condition(this, condition)?;
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

pub(crate) fn lower_match(
    context: &mut BodyLowering<'_>,
    condition: &HIRExpression,
    arms: &[(HIRPattern, HIRExpression)],
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let scrutinee = lower_expr(context, condition)?;
    let subject_ty = context.hole(span);
    let subject = context.declare_local(None, subject_ty, span);
    let arms = arms
        .iter()
        .map(|(pattern, body)| {
            context.scoped(|this| {
                let pattern = lower_pattern(this, pattern, &body.range)?;
                Ok((pattern, lower_expr(this, body)?))
            })
        })
        .collect::<CXResult<_>>()?;

    Ok(context.push(
        HMIRExprKind::Match {
            scrutinee,
            subject,
            arms,
        },
        span,
    ))
}

pub(crate) fn lower_switch(
    context: &mut BodyLowering<'_>,
    condition: &HIRExpression,
    body: &HIRExpression,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let condition = lower_expr(context, condition)?;
    let scope = ScopeKind::Switch { has_default: false };
    let body = context.scoped_as(scope, |this| lower_expr(this, body))?;

    Ok(context.push(HMIRExprKind::Switch { condition, body }, span))
}

pub(crate) fn lower_case(
    context: &mut BodyLowering<'_>,
    value: Option<&HIRExpression>,
    statement: &HIRExpression,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
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

// An omitted condition, as in 'for (;;)', always holds
fn lower_condition(
    context: &mut BodyLowering<'_>,
    condition: &HIRExpression,
) -> CXResult<HMIRExprID> {
    let span = &condition.range;
    let bool_ty = HMIRIntType::new(HMIRIntWidth::I1, false);
    if matches!(condition.kind, HIRExprKind::Void) {
        return Ok(context.int_literal(bool_ty, 1, span));
    }

    let value = lower_expr(context, condition)?;
    let target = context.type_ref(HMIRExprTypeKind::Int(bool_ty), span);

    Ok(context.coerce(HMIRCoerceMode::Truthy, value, target, span))
}
