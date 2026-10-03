use cx_hir::ast::{
    expression::{HIRExprKind, HIRExpression},
    pattern::HIRPattern,
};
use cx_hmir::{
    HMIRBlockKind, HMIRCoerceMode, HMIRConstant, HMIRExprID, HMIRExprKind, HMIRIntWidth,
    HMIRTypeDesc,
};
use cx_tokens::TokenRange;

use crate::{expr::lower_expr, expr::pattern::lower_pattern};

use crate::body::BodyLowering;

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
    block: &[HIRExpression],
    cases: &[(HIRExpression, usize)],
    default_case: Option<usize>,
    span: &TokenRange,
) -> HMIRExprID {
    let condition = lower_expr(cx, condition);
    let mut starts = cases.iter().map(|(_, start)| *start).collect::<Vec<_>>();
    starts.extend(default_case);
    starts.push(block.len());
    starts.sort_unstable();
    starts.dedup();

    cx.scoped(|this| {
        let segment = |this: &mut BodyLowering<'_>, start: usize| {
            let end = starts
                .iter()
                .copied()
                .find(|boundary| *boundary > start)
                .unwrap_or(block.len());
            let statements = block[start..end]
                .iter()
                .map(|statement| lower_expr(this, statement))
                .collect();
            let segment_span = block
                .get(start)
                .map(HIRExpression::token_range)
                .unwrap_or(span);
            this.block(HMIRBlockKind::Sequence, statements, segment_span)
        };

        let mut ordered = cases.iter().collect::<Vec<_>>();
        ordered.sort_by_key(|(_, start)| *start);
        let cases = ordered
            .iter()
            .enumerate()
            .map(|(index, (value, start))| {
                let case_span = value.token_range();
                let value = lower_expr(this, value);
                let shares_segment = ordered
                    .get(index + 1)
                    .is_some_and(|(_, next)| next == start)
                    || default_case == Some(*start);
                let body = if shares_segment {
                    this.block(HMIRBlockKind::Sequence, Vec::new(), case_span)
                } else {
                    segment(this, *start)
                };
                (value, body)
            })
            .collect();
        let default = default_case.map(|start| segment(this, start));

        this.push(
            HMIRExprKind::Switch {
                condition,
                cases,
                default,
            },
            span,
        )
    })
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
