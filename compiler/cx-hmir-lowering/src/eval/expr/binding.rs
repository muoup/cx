use cx_hmir::{
    HMIRBody, HMIROp, HMIROwnershipOp,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{
    CXResult,
    catalogue::{self, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{FnContext, LocalLoweringContext, runtime::Emission},
    eval::{
        EvalResult, coerce,
        control::{self, closing},
        deduce, eval, eval_type, expr,
        flow::{self, ComptimeStop, EvalExit, Evaluation, RuntimeStop, StopReason},
        staged, types,
    },
    log::hmir_error,
    lower::mir,
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
};

pub(crate) fn eval_local(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    local: HMIRLocalID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let Some(value) = context.get_local(&local).cloned() else {
        if context.is_runtime() {
            todo!("local {local} has no binding in the evaluating context")
        }
        let subject = match body.local(local).name() {
            Some(name) => format!("variable '{name}'"),
            None => "this variable".into(),
        };
        return Err(hmir_error(span, &catalogue::mir::COMPTIME_UNAVAILABLE, subject).into());
    };

    if let FnContext::Runtime(runtime) = context.context_mut()
        && let HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) = value.kind()
    {
        runtime.slot_mut(*slot).observe();
    }
    match context.env().reference_inner(value.ty()) {
        Some(inner) if context.env().is_reference(inner) => {
            Ok(Evaluation::Ready(coerce::implicit(context, value, inner, span)?))
        }
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
) -> EvalResult<Evaluation<HMIRValue>> {
    let local = body.local(local_id);
    if local.comptime() && context.is_runtime() {
        todo!("comptime let binding in a runtime context");
    }

    let declared = eval_type(context, body, local.ty())?;
    if let Some(ty) = declared.known().copied()
        && context.is_runtime()
        && staged::is_code(context, ty)
    {
        let Some(initializer) = initializer else {
            let kind = ("staged local".into(), "a declaration without an initializer".into());
            return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, kind).into());
        };
        let value = staged::argument(context, body, initializer, ty)?;
        let span = span.clone();
        return value.map(context, move |context, value| {
            emit::bind_local(context, local_id, value);
            Ok(expr::unit(context, &span))
        });
    }
    let storage = match declared.known().copied() {
        Some(ty) if context.complete_object(ty) => Some(declare(context, body, local_id, ty, span)?),
        _ => None,
    };
    let initialized = storage.is_some()
        && declared.known().is_some_and(|ty| !context.env().is_nocopy(*ty))
        && context.is_runtime();
    if initialized && let Some(storage) = &storage {
        emit::initialize(context, storage, None, span)?;
    }

    let Some(initializer) = initializer else {
        let ty = declared.complete(context, body, None)?;
        let storage = match storage {
            Some(storage) => storage,
            None => declare(context, body, local_id, ty, span)?,
        };
        if !initialized && !context.env().is_nocopy(ty) {
            emit::initialize(context, &storage, None, span)?;
        }
        return Ok(Evaluation::Ready(expr::unit(context, span)));
    };

    let is_runtime = context.is_runtime();
    let value = eval(context, body, initializer)?;
    let value = deduce::complete(context, body, value, declared.known().copied())?;
    let span = span.clone();
    let auto = matches!(body.expr(local.ty()).kind(), HMIRExprKind::Hole(_));
    value.and_then(context, body, None, move |context, body, value, _| {
        let value = match declared.known() {
            Some(ty) => coerce::implicit(context, value, *ty, &span)?,
            None => value,
        };
        let inferred = match auto {
            true => captured(context, body, initializer, &value, &span),
            false => coerce::value_type(context, value.ty(), &span),
        };
        let function = matches!(context.env().resolve_kind(inferred), HMIRTypeKind::Function(_));
        let inferred = if function && !types::comptime_only(context.env(), inferred) {
            types::pointer(context.env_mut(), inferred, &span)
        } else {
            inferred
        };
        declared.try_complete(context, body, Some(inferred))?.and_then(context, body, None, move |context, body, ty, _| {
            let value = coerce::implicit(context, value, ty, &span)?;
            let function = matches!(context.env().resolve_kind(ty), HMIRTypeKind::Function(_));
            if is_runtime && function && types::comptime_only(context.env(), ty) {
                emit::bind_local(context, local_id, value);
                return Ok(Evaluation::Ready(expr::unit(context, &span)));
            }
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
                emit::initialize(context, &storage, Some(&value), &span)?;
            }
            Ok(Evaluation::Ready(expr::unit(context, &span)))
        })
    })
}

// The type of the initializer as written: a place stands for its object, while a reference type that a local or
// a function declares is kept
fn captured(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    initializer: HMIRExprID,
    value: &HMIRValue,
    span: &TokenRange,
) -> HMIRTypeID {
    let declared = match body.expr(initializer).kind() {
        HMIRExprKind::Local(local) => context
            .get_local(local)
            .and_then(|binding| context.env().reference_inner(binding.ty()))
            .is_some_and(|inner| context.env().is_reference(inner)),
        HMIRExprKind::Call { .. } | HMIRExprKind::Splice { .. } => true,
        HMIRExprKind::Op(HMIROp::OwnershipOp(HMIROwnershipOp::Move(operand))) => {
            return captured(context, body, *operand, value, span);
        }
        _ => false,
    };
    match declared && context.env().is_reference(value.ty()) {
        true => value.ty(),
        false => coerce::value_type(context, value.ty(), span),
    }
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
    if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Function(_)) {
        let requirement = ("local variable".into(), "an object type".into(), Some(types::name(context.env(), ty)));
        return Err(hmir_error(span, &typecheck::TYPE_REQUIREMENT, requirement).into());
    }
    if !context.complete_object(ty) {
        let subject = format!("variable of type '{}'", types::name(context.env(), ty));
        return Err(hmir_error(span, &typecheck::INCOMPLETE_TYPE, subject).into());
    }

    let reference_type = types::reference(context.env_mut(), ty, None, local.span());
    let storage = emit::allocate(context, ty, local.name().cloned(), span)?.with_ty(reference_type);
    emit::bind_local(context, local_id, storage.clone());

    Ok(storage)
}

pub(crate) fn eval_block(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    statements: &[HMIRExprID],
    scoped: bool,
    yields: bool,
) -> EvalResult<Evaluation<HMIRValue>> {
    if !scoped {
        return eval_statements(context, body, span, statements, true);
    }

    let scope = emit::open_scope(context, span);
    if yields {
        match context.context_mut() {
            FnContext::Runtime(runtime) => runtime.scopes_mut().control_mut(scope).begin_yield(),
            FnContext::Comptime(comptime) => comptime.scopes_mut().control_mut(scope).begin_yield(),
        }
    }
    let result = eval_statements(context, body, span, statements, yields);
    let result = if yields { yielded(context, body, scope, result, span) } else { result };
    closing(context, body, result, span)
}

fn yielded(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    scope: usize,
    result: EvalResult<Evaluation<HMIRValue>>,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    if !context.is_runtime() {
        return flow::catch(result, |stop| match stop {
            StopReason::Comptime(ComptimeStop::Yield(value)) => Ok(Evaluation::Ready(value)),
            stop => Err(EvalExit::Stop(stop)),
        });
    }
    let span = span.clone();
    flow::catch_diverged(result)?.and_then(context, body, None, move |context, _, value, _| {
        let block = mir::current_block(context);
        let mut exits = context.runtime_mut().scopes_mut().control_mut(scope).end_yield();
        match value {
            Some(value) if exits.is_empty() => return Ok(Evaluation::Ready(value)),
            Some(value) => exits.push((block, value)),
            None => {}
        }
        let env = context.env();
        let voids = exits.iter().filter(|(_, value)| env.is_void(value.ty())).count();
        if voids != 0 && voids != exits.len() {
            let subject = "every path of an expression block".into();
            return Err(hmir_error(&span, &typecheck::MISSING_YIELD, subject).into());
        }
        control::merge(context, exits, &span)
    })
}

fn eval_statements(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    statements: &[HMIRExprID],
    yields: bool,
) -> EvalResult<Evaluation<HMIRValue>> {
    let mut dead = false;
    for (index, statement) in statements.iter().enumerate() {
        let result = eval(context, body, *statement);
        if matches!(&result, Err(EvalExit::Stop(StopReason::Runtime(RuntimeStop::Diverged)))) {
            dead = true;
            unreachable(context);
            continue;
        }
        let yielded = yields && index + 1 == statements.len();
        if yielded && dead && !reachable(context) {
            result?;
            return flow::diverged();
        }
        if yielded {
            let result = result?;
            let partial = result.known().filter(|value| value.as_type().is_some()).cloned();
            let span = span.clone();
            return result.and_then(context, body, partial, move |context, _, value, _| {
                Ok(Evaluation::Ready(coerce::std_rvalue(context, value, &span)?))
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
            result => {
                let value = (result)?.ready()?;
                if !context.env().is_reference(value.ty()) && context.env().is_nodrop(value.ty()) {
                    let ty = types::name(context.env(), value.ty());
                    let span = body.expr(*statement).span();
                    return Err(hmir_error(span, &typecheck::NODROP_DISCARDED, ty).into());
                }
            }
        };
    }
    if dead && !reachable(context) {
        return flow::diverged();
    }
    Ok(Evaluation::Ready(expr::unit(context, span)))
}

// Code after a divergence is still checked, in a block nothing jumps to unless a label or case inside it is entered
fn unreachable(context: &mut LocalLoweringContext) {
    if !context.is_unevaluated() {
        let block = mir::block(context, "unreachable");
        mir::switch_to(context, block);
    }
}

fn reachable(context: &LocalLoweringContext) -> bool {
    !context.is_unevaluated() && context.runtime().builder().current_block_reachable()
}

pub(crate) fn eval_scoped(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let node = body.expr(expr);
    if matches!(node.kind(), HMIRExprKind::Block { scoped: true, .. }) {
        return eval(context, body, expr);
    }

    let span = node.span();
    emit::open_scope(context, span);
    let result = eval_statements(context, body, span, &[expr], true);
    closing(context, body, result, span)
}
