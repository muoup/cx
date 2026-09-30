use cx_hir::ast::{expression::HIRExpression, pattern::HIRPattern};
use cx_hmir::{
    HMIRBlockKind, HMIRCoerceMode, HMIRConstant, HMIRExprID, HMIRExprKind, HMIRIntWidth,
    HMIRTypeDesc,
};
use cx_tokens::TokenRange;

use crate::body::BodyLowering;

impl BodyLowering<'_> {
    pub(super) fn lower_if(
        &mut self,
        condition: &HIRExpression,
        then_branch: &HIRExpression,
        else_branch: Option<&HIRExpression>,
        span: &TokenRange,
    ) -> HMIRExprID {
        let (condition, then_branch) = self.scoped(|this| {
            let condition = this.lower_condition(condition);
            (condition, this.scoped(|this| this.lower_expr(then_branch)))
        });
        let else_branch = else_branch.map(|branch| self.scoped(|this| this.lower_expr(branch)));
        self.push(
            HMIRExprKind::If {
                condition,
                then_branch,
                else_branch,
            },
            span,
        )
    }

    pub(super) fn lower_while(
        &mut self,
        condition: &HIRExpression,
        body: &HIRExpression,
        pre_eval: bool,
        span: &TokenRange,
    ) -> HMIRExprID {
        let condition = self.lower_condition(condition);
        let body = self.scoped(|this| this.lower_expr(body));
        self.push(
            HMIRExprKind::While {
                condition,
                body,
                pre_eval,
            },
            span,
        )
    }

    pub(super) fn lower_for(
        &mut self,
        init: &HIRExpression,
        condition: &HIRExpression,
        increment: &HIRExpression,
        body: &HIRExpression,
        span: &TokenRange,
    ) -> HMIRExprID {
        self.scoped(|this| {
            let init = this.lower_expr(init);
            let condition = this.lower_condition(condition);
            let increment = this.lower_expr(increment);
            let body = this.scoped(|this| this.lower_expr(body));
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

    pub(super) fn lower_match(
        &mut self,
        condition: &HIRExpression,
        arms: &[(HIRPattern, HIRExpression)],
        span: &TokenRange,
    ) -> HMIRExprID {
        let scrutinee = self.lower_expr(condition);
        let subject_ty = self.hole(span);
        let subject = self.declare_local(None, subject_ty, span);
        let arms = arms
            .iter()
            .map(|(pattern, body)| {
                self.scoped(|this| {
                    let pattern = this.lower_pattern(pattern, &body.range);
                    (pattern, this.lower_expr(body))
                })
            })
            .collect();
        self.push(
            HMIRExprKind::Match {
                scrutinee,
                subject,
                arms,
            },
            span,
        )
    }

    pub(super) fn lower_switch(
        &mut self,
        condition: &HIRExpression,
        block: &[HIRExpression],
        cases: &[(HIRExpression, usize)],
        default_case: Option<usize>,
        span: &TokenRange,
    ) -> HMIRExprID {
        let condition = self.lower_expr(condition);
        let mut starts = cases.iter().map(|(_, start)| *start).collect::<Vec<_>>();
        starts.extend(default_case);
        starts.push(block.len());
        starts.sort_unstable();
        starts.dedup();

        self.scoped(|this| {
            let segment = |this: &mut Self, start: usize| {
                let end = starts
                    .iter()
                    .copied()
                    .find(|boundary| *boundary > start)
                    .unwrap_or(block.len());
                let statements = block[start..end]
                    .iter()
                    .map(|statement| this.lower_expr(statement))
                    .collect();
                this.block(HMIRBlockKind::Sequence, statements, span)
            };

            let mut ordered = cases.iter().collect::<Vec<_>>();
            ordered.sort_by_key(|(_, start)| *start);
            let cases = ordered
                .iter()
                .enumerate()
                .map(|(index, (value, start))| {
                    let value = this.lower_expr(value);
                    let shares_segment = ordered
                        .get(index + 1)
                        .is_some_and(|(_, next)| next == start);
                    let body = if shares_segment {
                        this.block(HMIRBlockKind::Sequence, Vec::new(), span)
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

    fn lower_condition(&mut self, condition: &HIRExpression) -> HMIRExprID {
        let span = &condition.range;
        if matches!(condition.kind, cx_hir::ast::expression::HIRExprKind::Void) {
            return self.push(HMIRExprKind::Constant(HMIRConstant::Bool(true)), span);
        }
        let value = self.lower_expr(condition);
        let target = self.type_constant(
            HMIRTypeDesc::Int {
                width: HMIRIntWidth::I1,
                signed: false,
            },
            span,
        );
        self.coerce(HMIRCoerceMode::Truthy, value, target, span)
    }
}
