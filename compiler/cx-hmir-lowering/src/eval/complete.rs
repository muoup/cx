use cx_hmir::{
    HMIRAggregateOp, HMIRBody, HMIROp,
    body::HMIRHoleID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, context::HMIRTypeContext},
};
use cx_log::catalogue::typecheck;

use crate::{
    env::function::{LocalLoweringContext, elaboration::ElaborationScopeID},
    eval::{EvalResult, eval},
    log::hmir_error,
    value::HMIRValue,
};

pub(crate) enum Completion {
    Ready(HMIRValue),
    Pending(Pending),
}

pub(crate) struct Pending {
    expr: HMIRExprID,
    scope: ElaborationScopeID,
    holes: Vec<HMIRHoleID>,
}

pub(crate) fn elaborate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> EvalResult<Completion> {
    let mut holes = Vec::new();
    contextual_holes(context, body, expr, &mut holes);
    if holes.is_empty() {
        return Ok(Completion::Ready(eval(context, body, expr)?));
    }

    Ok(Completion::Pending(Pending {
        expr,
        scope: context.elaboration().id(),
        holes,
    }))
}

impl Completion {
    pub(crate) fn complete(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBody,
        expected: Option<HMIRTypeID>,
    ) -> EvalResult<HMIRValue> {
        let pending = match self {
            Self::Ready(value) => return Ok(value),
            Self::Pending(pending) => pending,
        };
        assert_eq!(
            pending.scope,
            context.elaboration().id(),
            "pending expression completed outside of the scope that elaborated it"
        );

        let Some(expected) = expected else {
            let span = body.expr(pending.expr).span();
            let subject = "the initialized type".into();
            return Err(hmir_error(span, &typecheck::CANNOT_INFER, subject).into());
        };
        let expected = context.env().reference_inner(expected).unwrap_or(expected);

        let mark = context.elaboration_mut().bind(&pending.holes, expected);
        let result = eval(context, body, pending.expr);
        context.elaboration_mut().release(mark);

        result
    }
}

fn contextual_holes(
    context: &LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    holes: &mut Vec<HMIRHoleID>,
) {
    match body.expr(expr).kind() {
        HMIRExprKind::Op(HMIROp::AggregateOp(HMIRAggregateOp::Initialize { ty, .. })) => {
            if let HMIRExprKind::Hole(hole) = body.expr(*ty).kind()
                && context.elaboration().hole(*hole).is_none()
            {
                holes.push(*hole);
            }
        }
        HMIRExprKind::Block {
            statements,
            scoped,
            yields,
        } if *yields || !*scoped => {
            if let Some(last) = statements.last() {
                contextual_holes(context, body, *last, holes);
            }
        }
        HMIRExprKind::If {
            then_branch,
            else_branch: Some(else_branch),
            ..
        } => {
            contextual_holes(context, body, *then_branch, holes);
            contextual_holes(context, body, *else_branch, holes);
        }
        _ => {}
    }
}
