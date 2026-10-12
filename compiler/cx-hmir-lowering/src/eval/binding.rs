use cx_hmir::{
    HMIRBody, HMIROp, HMIROwnershipOp,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeKind, context::HMIRTypeContext},
    value::HMIRValue,
};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{FnContext, FnLoweringContext},
    eval::{EvalResult, coerce::convert, constant, eval, eval_type},
};

pub(crate) fn eval_local(
    context: &mut FnLoweringContext,
    span: &TokenRange,
    local: HMIRLocalID,
) -> EvalResult<HMIRValue> {
    let Some(value) = context.get_local(&local).cloned() else {
        todo!("local {local} has no binding in the evaluating context")
    };

    Ok(emit::referent(context, value, span)?)
}

pub(crate) fn eval_let(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    local: HMIRLocalID,
    initializer: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let decl = body.local(local);
    if decl.comptime() && matches!(context.context(), FnContext::Runtime(_)) {
        todo!("comptime let binding in a runtime context");
    }

    let ty = eval_type(context, body, decl.ty())?;
    if matches!(context.env().resolve_type_id(ty).kind(), HMIRTypeKind::Void) {
        todo!("local of type void");
    }

    let allocates = initializer.is_some_and(|initializer| {
        matches!(
            body.expr(initializer).kind(),
            HMIRExprKind::Op(HMIROp::OwnershipOp(HMIROwnershipOp::Allocate(_)))
        )
    });

    let value = match initializer {
        Some(initializer) if !allocates => {
            let value = eval(context, body, initializer)?;
            Some(convert(context, value, ty, body.expr(initializer).span())?)
        }
        _ => None,
    };

    let place = emit::allocate(context, ty, decl.name().cloned(), decl.nodrop(), span)?;
    if let Some(value) = value {
        emit::write(context, &place, value, true, span)?;
    }

    context.insert_local(local, place);
    Ok(constant::unit(context, span))
}

pub(crate) fn eval_block(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    statements: &[HMIRExprID],
) -> EvalResult<HMIRValue> {
    emit::open_scope(context, span.clone());
    let result = eval_statements(context, body, span, statements);
    emit::close_scope(context, span.clone());

    result
}

fn eval_statements(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    statements: &[HMIRExprID],
) -> EvalResult<HMIRValue> {
    let mut result = None;
    for statement in statements {
        result = Some(eval(context, body, *statement)?);
    }

    match result {
        Some(value) => Ok(emit::read(context, value, span)?),
        None => Ok(constant::unit(context, span)),
    }
}

pub(crate) fn eval_scoped(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let node = body.expr(expr);
    if matches!(node.kind(), HMIRExprKind::Block { .. }) {
        return eval(context, body, expr);
    }

    eval_block(context, body, node.span(), &[expr])
}
