use cx_hmir::{
    HMIRBody,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, context::HMIRTypeContext},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        EvalResult,
        coerce, complete,
        eval, eval_type,
        flow::{Evaluation, ready},
        literal, storage, types,
    },
    log::hmir_error,
    value::HMIRValue,
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
                let storage = match storage {
                    Some(storage) => storage,
                    None => declare(context, body, local_id, ty, &span)?,
                };
                if initialized {
                    emit::write(context, &storage, &value, &span)?;
                } else {
                    storage::initialize(context, &storage, &value, &span)?;
                }
                Ok(Evaluation::Ready(literal::unit(context, &span)))
            },
        )
    })
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
    match result {
        Ok(result) => {
            let span = span.clone();
            Ok(result.finish(context, move |context| {
                emit::close_scope(context, span.clone())
            }))
        }
        Err(error) => {
            emit::close_scope(context, span.clone());
            Err(error)
        }
    }
}

fn eval_statements(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    statements: &[HMIRExprID],
    yields: bool,
) -> EvalResult<HMIRValue> {
    for (index, statement) in statements.iter().enumerate() {
        let lifts = match context.context() {
            FnContext::Runtime(runtime) => Some(runtime.lift_count()),
            FnContext::Comptime(_) => None,
        };
        let result = eval(context, body, *statement);
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
        ready!(result);
        if let Some(lifts) = lifts {
            context.runtime_mut().release_lifts(lifts);
        }
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
