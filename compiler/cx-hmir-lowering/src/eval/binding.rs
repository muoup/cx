use cx_hmir::{
    HMIRBody,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{FnContext, LocalLoweringContext, runtime::Emission},
    eval::{
        EvalResult, coerce, complete, defer, eval, eval_type,
        flow::{self, Evaluation, RuntimeStop, StopReason, ready},
        literal, staged, storage, switch, types,
    },
    log::hmir_error,
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
};

pub(crate) fn eval_local(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    local: HMIRLocalID,
) -> EvalResult<HMIRValue> {
    let Some(value) = context.get_local(&local).cloned() else {
        match context.context() {
            FnContext::Comptime(_) => {
                let subject = match body.local(local).name() {
                    Some(name) => format!("variable '{name}'"),
                    None => "this variable".into(),
                };
                return Err(hmir_error(span, &mir::COMPTIME_UNAVAILABLE, subject).into());
            }
            FnContext::Runtime(_) => {
                todo!("local {local} has no binding in the evaluating context")
            }
        }
    };

    if let FnContext::Runtime(runtime) = context.context_mut()
        && let HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) = value.kind()
    {
        runtime.slot_mut(*slot).observe();
    }
    let stored = context.env().reference_inner(value.ty());
    match stored {
        Some(inner) if context.env().is_reference(inner) => Ok(Evaluation::Ready(
            coerce::implicit(context, value, inner, span)?,
        )),
        _ => Ok(Evaluation::Ready(value)),
    }
}

// A declaration is a statement, and evaluates to unit. Each evaluation binds a fresh instance of the local, so a
// binding left by an earlier evaluation of the same declaration, as a comptime loop makes, is forgotten first
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

    let declared = eval_type(context, body, local.ty())?;
    if let Some(ty) = declared.known().copied()
        && matches!(context.context(), FnContext::Runtime(_))
        && matches!(context.env().resolve_kind(ty), HMIRTypeKind::StagedExpr { .. })
    {
        let Some(initializer) = initializer else {
            let kind = ("staged local".into(), "a declaration without an initializer".into());
            return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, kind));
        };
        let value = staged::argument(context, body, initializer, ty)?;
        let span = span.clone();
        return value.and_then(context, body, None, move |context, _, value, _| {
            emit::storage::bind_local(context, local_id, value);
            Ok(Evaluation::Ready(literal::unit(context, &span)))
        });
    }
    let storage = match declared.known().copied() {
        Some(ty) if context.complete_object(ty) => {
            Some(declare(context, body, local_id, ty, span)?)
        }
        _ => None,
    };
    let initialized = storage.is_some()
        && declared
            .known()
            .is_some_and(|ty| !context.env().is_nocopy(*ty))
        && matches!(context.context(), FnContext::Runtime(_));
    if initialized && let Some(storage) = &storage {
        emit::initialize(context, storage, span);
    }

    let Some(initializer) = initializer else {
        let ty = ready!(declared.complete(context, body, None));
        let storage = match storage {
            Some(storage) => storage,
            None => declare(context, body, local_id, ty, span)?,
        };
        if !initialized && !context.env().is_nocopy(ty) {
            emit::initialize(context, &storage, span);
        }
        return Ok(Evaluation::Ready(literal::unit(context, span)));
    };

    let is_runtime = matches!(context.context(), FnContext::Runtime(_));
    let value = eval(context, body, initializer)?;
    let value = complete::value(context, body, value, declared.known().copied())?;
    let span = span.clone();
    value.and_then(context, body, None, move |context, body, value, _| {
        let value = match declared.known() {
            Some(ty) => coerce::implicit(context, value, *ty, &span)?,
            None => value,
        };
        let inferred = coerce::value_type(context, value.ty(), &span);
        declared.complete(context, body, Some(inferred))?.and_then(
            context,
            body,
            None,
            move |context, body, ty, _| {
                let value = coerce::implicit(context, value, ty, &span)?;
                let reserved = storage.is_some();
                let storage = match storage {
                    Some(storage) => storage,
                    None => declare(context, body, local_id, ty, &span)?,
                };
                let reusable = is_runtime
                    && (!reserved || !observed(context, &storage))
                    && context.runtime().emission() == Emission::Enabled
                    && emit::scratch_held(context, &value);
                if reusable {
                    emit::adopt(context, &storage, &value, &span)?;
                } else if initialized {
                    emit::write(context, &storage, &value, &span)?;
                } else {
                    storage::initialize(context, &storage, &value, &span)?;
                }
                Ok(Evaluation::Ready(literal::unit(context, &span)))
            },
        )
    })
}

fn observed(context: &LocalLoweringContext, storage: &HMIRValue) -> bool {
    match storage.kind() {
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => context.runtime().slot(*slot).observed(),
        _ => true,
    }
}

fn declare(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    local_id: HMIRLocalID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let local = body.local(local_id);
    types::validate::object(context.env(), ty, span)?;
    if !context.complete_object(ty) {
        let subject = format!("variable of type '{}'", types::name(context.env(), ty));
        return Err(hmir_error(span, &typecheck::INCOMPLETE_TYPE, subject));
    }

    let reference_type = types::reference(context.env_mut(), ty, local.span());
    let storage = emit::allocate(context, ty, local.name().cloned(), span)?.with_ty(reference_type);
    emit::storage::bind_local(context, local_id, storage.clone());

    Ok(storage)
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

    emit::open_scope(context, span.clone());
    let result = eval_statements(context, body, span, statements, yields);
    let span = span.clone();
    flow::finally(context, body, result, move |context, _, completed| {
        defer::close(context, completed, &span)
    })
}

fn eval_statements(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    statements: &[HMIRExprID],
    yields: bool,
) -> EvalResult<HMIRValue> {
    let mut stopped = false;
    for (index, statement) in statements.iter().enumerate() {
        if stopped && !switch::contains_case(context, body, *statement) {
            continue;
        }
        let result = eval(context, body, *statement);
        stopped = switch::in_switch(context)
            && matches!(
                result,
                Ok(Evaluation::Stop(StopReason::Runtime(RuntimeStop::Diverged)))
            );
        if stopped {
            continue;
        }
        let yielded = yields && index + 1 == statements.len();
        if yielded {
            let result = result?;
            let partial = result
                .known()
                .filter(|value| super::type_constant(value).is_some())
                .cloned();
            let span = span.clone();
            return result.and_then(context, body, partial, move |context, _, value, _| {
                Ok(Evaluation::Ready(coerce::std_rvalue(
                    context, value, &span,
                )?))
            });
        }
        match result {
            Ok(Evaluation::Incomplete(pending)) if pending.is_yielding() => {
                let remaining = statements[index + 1..].to_vec();
                let span = span.clone();
                let pending = Evaluation::Incomplete(pending);
                return pending.and_then(context, body, None, move |context, body, _, _| {
                    eval_statements(context, body, &span, &remaining, yields)
                });
            }
            result => ready!(result),
        };
    }
    if stopped {
        return Ok(Evaluation::Stop(RuntimeStop::Diverged.into()));
    }
    Ok(Evaluation::Ready(literal::unit(context, span)))
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
