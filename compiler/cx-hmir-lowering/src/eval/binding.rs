use cx_hmir::{
    HMIRBody,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeKind, context::HMIRTypeContext},
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime}, env::function::{FnContext, LocalLoweringContext}, eval::{
        EvalResult, aggregate::eval_initializer, coerce::{self, convert}, constant, eval, eval_type, flow::EvalStop,
    },
};

pub(crate) fn eval_local(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    local: HMIRLocalID,
) -> EvalResult<HMIRValue> {
    let value = match context.get_local(&local).cloned() {
        Some(value) => value,
        None if matches!(context.context(), FnContext::Runtime(_)) => {
            let decl = body.local(local);
            let ty = eval_type(context, body, decl.ty())?;
            let value = emit::allocate(
                context,
                ty,
                decl.name().cloned(),
                decl.nodrop(),
                decl.span(),
            )?;
            if !context
                .env()
                .resolve_type_id(ty)
                .semantics()
                .move_semantics
                .is_nocopy()
            {
                runtime::initialize(context, &value, decl.span());
            }
            context.insert_local(local, value.clone());
            value
        }
        None => todo!("local {local} has no binding in the evaluating context"),
    };

    let value = emit::referent(context, value, span)?;
    Ok(emit::reference(context, &value, span)?)
}

pub(crate) fn eval_let(
    context: &mut LocalLoweringContext,
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
    if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Void) {
        todo!("local of type void");
    }

    match initializer {
        Some(initializer) => {
            let value = eval_initializer(context, body, initializer, ty)?;
            let coerced = convert(context, value, ty, span)?;

            // Since we own the rhs, if it is addressable, we can simply alias the initialization
            // to that value without needing to allocate.
            if matches!(coerced.kind(), HMIRValueKind::PlaceRef(HMIRPlace::Local(_))) {
                emit::bind_local(context, local, coerced.clone())?;

                return Ok(coerced);
            }
            
            let allocation = emit::allocate(context, ty, decl.name().cloned(), decl.nodrop(), decl.span())
                .map_err(EvalStop::from)?;

            emit::write(context, &allocation, coerced, true, span)?;
            emit::bind_local(context, local, allocation.clone())?;
 
            Ok(allocation)
        }

        None => {
            let allocation = emit::allocate(context, ty, decl.name().cloned(), decl.nodrop(), decl.span())
                .map_err(EvalStop::from)?;

            emit::bind_local(context, local, allocation.clone())?;

            Ok(allocation)
        }
    }
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
        _ => Ok(constant::unit(context, span)),
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
