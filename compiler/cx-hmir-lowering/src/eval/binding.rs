use cx_hmir::{
    HMIRBody,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeKind, context::HMIRTypeContext},
};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, Transfer, runtime},
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        EvalResult,
        aggregate::eval_initializer,
        coerce::{self},
        eval, eval_type,
        literal, types,
    },
    value::HMIRValue,
};

pub(crate) fn eval_local(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    local: HMIRLocalID,
) -> EvalResult<HMIRValue> {
    let Some(value) = context.get_local(&local).cloned() else {
        todo!("local {local} has no binding in the evaluating context")
    };

    let stored = context.env().reference_inner(value.ty());
    match stored {
        Some(inner) if context.env().is_reference(inner) => {
            Ok(coerce::implicit(context, value, inner, span)?)
        }
        _ => Ok(value),
    }
}

// A declaration is a statement, and evaluates to unit. Each evaluation binds a fresh instance of the local, so a
// binding left by an earlier evaluation of the same declaration, as a comptime loop makes, is forgotten first. The
// initializer is evaluated before the local is bound, and may itself name the local, which declares its storage early
pub(crate) fn eval_let(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    local_id: HMIRLocalID,
    initializer: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let local = body.local(local_id);
    if local.comptime() && matches!(context.context(), FnContext::Runtime(_)) {
        todo!("comptime let binding in a runtime context");
    }

    let ty = eval_type(context, body, local.ty())?;
    if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Void) {
        todo!("local of type void");
    }

    let reference_type = types::reference(context.env_mut(), ty, local.span());

    let storage = match initializer {
        Some(initializer) => {
            let value = eval_initializer(context, body, initializer, ty)?;
            let value = coerce::implicit(context, value, ty, span)?;

            if !context.env().is_reference(ty) && emit::storage::is_place(context, &value) {
                value.with_ty(reference_type)
            } else {
                let allocation = emit::allocate(context, ty, None, span)?;
                let allocation = allocation.with_ty(reference_type);
                emit::transfer(context, &allocation, value, Transfer::Init, span)?;
                allocation
            }
        }
        None => emit::allocate(context, ty, None, span)?.with_ty(reference_type),
    };
    emit::storage::bind_local(context, local_id, storage);

    Ok(literal::unit(context, span))
}

pub(crate) fn eval_block(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    statements: &[HMIRExprID],
    scoped: bool,
    yields: bool,
) -> EvalResult<HMIRValue> {
    if !scoped {
        return eval_statements(context, body, span, statements, true);
    }

    let parent = match context.context() {
        FnContext::Runtime(runtime) => runtime.open_scopes().last().copied(),
        FnContext::Comptime(_) => None,
    };
    emit::open_scope(context, span.clone());
    let result = eval_statements(context, body, span, statements, yields);
    if let (Some(scope), Ok(value)) = (parent, &result)
        && context.env().reference_inner(value.ty()).is_none()
    {
        runtime::relocate(context, value, scope, None);
    }
    emit::close_scope(context, span.clone());

    result
}

fn eval_statements(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    statements: &[HMIRExprID],
    yields: bool,
) -> EvalResult<HMIRValue> {
    let mut result = None;
    for statement in statements {
        result = Some(eval(context, body, *statement)?);
    }

    match result {
        Some(value) if yields => Ok(coerce::std_rvalue(context, value, span)?),
        _ => Ok(literal::unit(context, span)),
    }
}

pub(crate) fn eval_scoped(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let node = body.expr(expr);
    if matches!(node.kind(), HMIRExprKind::Block { scoped: true, .. }) {
        return eval(context, body, expr);
    }

    eval_block(context, body, node.span(), &[expr], true, true)
}
