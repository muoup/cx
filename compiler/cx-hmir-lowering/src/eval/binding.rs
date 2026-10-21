use cx_hmir::{
    HMIRBody, body::{HMIRLocal, HMIRLocalID}, expr::{HMIRExprID, HMIRExprKind}, ty::{HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext},
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        EvalResult,
        aggregate::eval_initializer,
        coerce::{self},
        eval, eval_type,
        flow::EvalStop,
        literal,
    },
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
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

    Ok(value)
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

    let reference_type = context.env_mut().resolver_mut().realize_type(
        HMIRType::new(
            HMIRTypeKind::ReferenceTo(ty),
            HMIRTypeSemantics::default(),
            local.span().clone(),
            None
        )
    );

    println!("eval_let: local {local_id:?} of type {ty:?} with reference type {reference_type:?}");

    if let Some(initializer) = initializer {
        let value = eval_initializer(context, body, initializer, ty)
            .and_then(|value| coerce::cast(context, value, ty, span).map_err(EvalStop::from))?;

        if matches!(value.kind(), HMIRValueKind::PlaceRef(HMIRPlace::Local(_))) {
            let value = value.clone().with_ty(reference_type);
            
            emit::storage::bind_local(context, local_id, value.clone());

            println!("Alloc: {:?}", value);
            
            return Ok(value);
        }

        let allocation =
            emit::storage::allocate(context, ty, None, span).map_err(EvalStop::from)?;
        emit::storage::write(context, &allocation, &value, span).map_err(EvalStop::from)?;
    
        return Ok(allocation.with_ty(reference_type));
    };

    emit::storage::allocate(context, ty, None, span)
        .map(|allocation| allocation.with_ty(reference_type))
        .map_err(EvalStop::from)
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
        Some(value) if yields => Ok(coerce::value(context, value, span)?),
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
