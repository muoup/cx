use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBody, HMIRCoerceMode, HMIROp, HMIRUnaryOp,
    expr::{HMIRExprID, HMIRExprKind},
};
use cx_mir::{MIRBasicBlockID, MIRBlockTarget};

use crate::{
    emit::runtime,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        aggregate::pattern,
        coerce, eval,
        flow::{Evaluation, ready},
    },
};

pub(crate) fn branch(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    matched: MIRBasicBlockID,
    unmatched: MIRBasicBlockID,
) -> EvalResult<()> {
    let node = body.expr(expr);
    match node.kind() {
        HMIRExprKind::Op(HMIROp::Coerce {
            mode: HMIRCoerceMode::Truthy,
            value,
            ..
        }) => {
            return branch(context, body, *value, matched, unmatched);
        }
        HMIRExprKind::Op(HMIROp::UnOp {
            op: HMIRUnaryOp::LNot,
            operand,
        }) => {
            return branch(context, body, *operand, unmatched, matched);
        }
        HMIRExprKind::Op(HMIROp::BinOp {
            op: HMIRBinaryOp::LAnd,
            lhs,
            rhs,
        }) => {
            let next = runtime::block(context, "condition.and");
            ready!(branch(context, body, *lhs, next, unmatched));
            runtime::switch_to(context, next);
            return branch(context, body, *rhs, matched, unmatched);
        }
        HMIRExprKind::Op(HMIROp::BinOp {
            op: HMIRBinaryOp::LOr,
            lhs,
            rhs,
        }) => {
            let next = runtime::block(context, "condition.or");
            ready!(branch(context, body, *lhs, matched, next));
            runtime::switch_to(context, next);
            return branch(context, body, *rhs, matched, unmatched);
        }
        HMIRExprKind::Op(HMIROp::AggregateOp(HMIRAggregateOp::Is { value, pattern })) => {
            ready!(pattern::eval_is(
                context,
                body,
                node.span(),
                *value,
                pattern,
                Some((matched, unmatched))
            ));
        }
        _ => {
            let value = ready!(eval(context, body, expr));
            let condition = coerce::condition(context, value, node.span())?;
            let condition = runtime::lower_value(context, &condition, node.span())?;
            runtime::branch(
                context,
                condition,
                MIRBlockTarget::new(matched),
                MIRBlockTarget::new(unmatched),
                node.span().clone(),
            );
        }
    }
    Ok(Evaluation::Ready(()))
}
