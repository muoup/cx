use cx_hir::ast::{
    expression::{HIRExprKind, HIRExpression},
    pattern::HIRPattern,
};
use cx_hmir::{HMIRCoerceMode, HMIRConstant, HMIRExprID, HMIRExprKind, HMIRIntWidth, HMIRTypeDesc};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;

use crate::{expr::lower_expr, expr::pattern::lower_pattern};

use crate::body::{BodyLowering, ScopeKind};

pub(crate) fn lower_if(
    cx: &mut BodyLowering<'_>,
    condition: &HIRExpression,
    then_branch: &HIRExpression,
    else_branch: Option<&HIRExpression>,
    span: &TokenRange,
) -> HMIRExprID {
    let (condition, then_branch) = cx.scoped(|this| {
        let condition = lower_condition(this, condition);
        (condition, this.scoped(|this| lower_expr(this, then_branch)))
    });
    let else_branch = else_branch.map(|branch| cx.scoped(|this| lower_expr(this, branch)));
    cx.push(
        HMIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        },
        span,
    )
}

pub(crate) fn lower_while(
    cx: &mut BodyLowering<'_>,
    condition: &HIRExpression,
    body: &HIRExpression,
    pre_eval: bool,
    span: &TokenRange,
) -> HMIRExprID {
    let condition = lower_condition(cx, condition);
    let body = cx.scoped(|this| lower_expr(this, body));
    cx.push(
        HMIRExprKind::While {
            condition,
            body,
            pre_eval,
        },
        span,
    )
}

pub(crate) fn lower_for(
    cx: &mut BodyLowering<'_>,
    init: &HIRExpression,
    condition: &HIRExpression,
    increment: &HIRExpression,
    body: &HIRExpression,
    span: &TokenRange,
) -> HMIRExprID {
    cx.scoped(|this| {
        let init = lower_expr(this, init);
        let condition = lower_condition(this, condition);
        let increment = lower_expr(this, increment);
        let body = this.scoped(|this| lower_expr(this, body));
        this.push(
            HMIRExprKind::For {
                init,
                condition,
                increment,
                body,
            },
            span,
        )
    })
}

pub(crate) fn lower_match(
    cx: &mut BodyLowering<'_>,
    condition: &HIRExpression,
    arms: &[(HIRPattern, HIRExpression)],
    span: &TokenRange,
) -> HMIRExprID {
    let scrutinee = lower_expr(cx, condition);
    let subject_ty = cx.hole(span);
    let subject = cx.declare_local(None, subject_ty, span);
    let arms = arms
        .iter()
        .map(|(pattern, body)| {
            cx.scoped(|this| {
                let pattern = lower_pattern(this, pattern, &body.range);
                (pattern, lower_expr(this, body))
            })
        })
        .collect();
    cx.push(
        HMIRExprKind::Match {
            scrutinee,
            subject,
            arms,
        },
        span,
    )
}

pub(crate) fn lower_switch(
    cx: &mut BodyLowering<'_>,
    condition: &HIRExpression,
    body: &HIRExpression,
    span: &TokenRange,
) -> HMIRExprID {
    let condition = lower_expr(cx, condition);
    let scope = ScopeKind::Switch { has_default: false };
    let body = cx.scoped_as(scope, |this| lower_expr(this, body));
    cx.push(HMIRExprKind::Switch { condition, body }, span)
}

pub(crate) fn lower_case(
    cx: &mut BodyLowering<'_>,
    value: Option<&HIRExpression>,
    statement: &HIRExpression,
    span: &TokenRange,
) -> HMIRExprID {
    if !cx.in_switch() {
        return cx.error(
            span,
            &typecheck::REQUIRED_CONTEXT,
            ("case label".into(), "a switch".into()),
        );
    }
    let value = match value {
        Some(value) => Some(lower_expr(cx, value)),
        None if cx.declare_switch_default() => None,
        None => {
            return cx.error(
                span,
                &typecheck::DUPLICATE_ITEM,
                ("default label".into(), "switch statement".into()),
            );
        }
    };
    let body = lower_expr(cx, statement);
    cx.push(HMIRExprKind::Case { value, body }, span)
}

fn lower_condition(cx: &mut BodyLowering<'_>, condition: &HIRExpression) -> HMIRExprID {
    let span = &condition.range;
    if matches!(condition.kind, HIRExprKind::Void) {
        return cx.push(HMIRExprKind::Constant(HMIRConstant::Bool(true)), span);
    }
    let value = lower_expr(cx, condition);
    let target = cx.type_constant(
        HMIRTypeDesc::Int {
            width: HMIRIntWidth::I1,
            signed: false,
        },
        span,
    );
    cx.coerce(HMIRCoerceMode::Truthy, value, target, span)
}
