use cx_hmir::{
    HMIRBody,
    body::{HMIRLocal, HMIRLocalID},
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit::{self, Transfer, runtime},
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        EvalResult,
        aggregate::eval_initializer,
        coerce::{self, convert},
        eval, eval_type, literal,
    },
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
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
            declare(context, local, decl, ty)?
        }
        None => todo!("local {local} has no binding in the evaluating context"),
    };

    let value = emit::referent(context, value, span)?;
    Ok(emit::reference(context, &value, span)?)
}

// Binds a local to fresh storage of its declared type. The storage of a copyable runtime value is live, with
// indeterminate contents, from its declaration on; a value with move semantics, or a comptime slot, holds nothing
// until a whole value is moved into it
fn declare(
    context: &mut LocalLoweringContext,
    local: HMIRLocalID,
    decl: &HMIRLocal,
    ty: HMIRTypeID,
) -> CXResult<HMIRValue> {
    let storage = emit::allocate(context, ty, decl.name().cloned(), decl.span())?;
    if matches!(context.context(), FnContext::Runtime(_)) && !context.env().is_nocopy(ty) {
        emit::initialize(context, &storage, decl.span());
    }
    context.insert_local(local, storage.clone());

    Ok(storage)
}

// A declaration is a statement, and evaluates to unit. Each evaluation binds a fresh instance of the local, so a
// binding left by an earlier evaluation of the same declaration, as a comptime loop makes, is forgotten first. The
// initializer is evaluated before the local is bound, and may itself name the local, which declares its storage early
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

    context.remove_local(local);
    let Some(initializer) = initializer else {
        declare(context, local, decl, ty)?;
        return Ok(literal::unit(context, span));
    };

    let scope = match context.context() {
        FnContext::Runtime(runtime) => runtime.open_scopes().last().copied(),
        FnContext::Comptime(_) => None,
    };
    let value = eval_initializer(context, body, initializer, ty)?;
    let value = convert(context, value, ty, span)?;

    // A value of a type that is not a reference is a temporary that the initializer owns, as naming any storage
    // yields a reference to it; owned storage of the declared type is adopted as the local's own
    let storage = match context.get_local(&local).cloned() {
        Some(storage) => {
            emit::transfer(context, &storage, value, Transfer::Init, span)?;
            storage
        }
        None if context.env().reference_inner(ty).is_none()
            && matches!(value.kind(), HMIRValueKind::PlaceRef(HMIRPlace::Local(_)))
            && emit::addressable(context, &value) =>
        {
            context.insert_local(local, value.clone());
            value
        }
        None => {
            let storage = emit::allocate(context, ty, decl.name().cloned(), decl.span())?;
            emit::transfer(context, &storage, value, Transfer::Init, span)?;
            context.insert_local(local, storage.clone());
            storage
        }
    };
    if let Some(scope) = scope {
        runtime::relocate(context, &storage, scope, decl.name().cloned());
    }

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
