use cx_hmir::{
    HMIRBody,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::context::HMIRTypeContext,
};
use cx_log::catalogue::{mir, typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        EvalResult,
        coerce::{self},
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

    let declared = eval_type(context, body, local.ty())?;
    let ty = match declared.known() {
        Some(ty) => *ty,
        None => {
            ready!(declared.complete(context, body, None));
            unreachable!("a declaration without known type information is incomplete or stopped")
        }
    };

    let value = match initializer {
        Some(initializer) => {
            let value = eval(context, body, initializer)?;
            let value = ready!(value.complete(context, body, Some(ty)));
            Some(coerce::implicit(context, value, ty, span)?)
        }
        None => None,
    };

    let ty = match &value {
        Some(value) => ready!(declared.complete(context, body, Some(value.ty()))),
        None => ready!(declared.complete(context, body, None)),
    };

    if !types::is_complete(context.env(), ty) {
        let subject = format!("variable of type '{}'", types::name(context.env(), ty));
        return Err(hmir_error(span, &typecheck::INCOMPLETE_TYPE, subject).into());
    }

    let reference_type = types::reference(context.env_mut(), ty, local.span());
    let local_storage = emit::allocate(context, ty, local.name().cloned(), span)
        .map(|allocate| allocate.with_ty(reference_type))?;

    if let Some(value) = value {
        storage::initialize(context, &local_storage, &value, span)?;
    } else if !context.env().semantics(ty).move_semantics.is_nocopy() {
        emit::initialize(context, &local_storage, span);
    }
    emit::storage::bind_local(context, local_id, local_storage);

    Ok(Evaluation::Ready(literal::unit(context, span)))
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
