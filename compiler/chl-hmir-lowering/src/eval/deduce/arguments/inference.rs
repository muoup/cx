use chl_hmir::{
    HMIRBodyContext,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRExprTypeKind, HMIRTypeID, context::HMIRTypeContext},
};
use chl_log::{CHLResult, catalogue::mir};
use chl_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        binding::UnboundLocal,
        call::dependent::Frame,
        comptime, eval, eval_type,
        flow::{self, EvalExit, Evaluation},
        staged, subject, types,
    },
    log::hmir_error,
    value::HMIRValue,
};

use super::{Deduction, Form, bind, matching::unify};

pub(crate) fn infer_from_types(
    deduction: &mut Deduction,
    context: &mut LocalLoweringContext,
    frame: &mut Frame,
    types: impl IntoIterator<Item = (usize, HMIRTypeID)>,
    span: &TokenRange,
) -> CHLResult<Option<Vec<Option<HMIRValue>>>> {
    let function = deduction.function;
    let body = function.body();
    let params = function.signature().params();
    for (index, ty) in types {
        let form = Form::new(body, *params[index].ty());
        unify(deduction, context, frame, form, ty, span)?;
    }
    if deduction.slots.values().any(|slot| slot.evidence.is_none()) {
        return Ok(None);
    }
    let mut values = vec![None; params.len()];
    flow::boundary(bind(deduction, context, frame, body, &mut values, span))?;
    Ok(Some(values))
}

pub(crate) fn infer(
    deduction: &mut Deduction,
    context: &mut LocalLoweringContext,
    frame: &mut Frame,
    body: &HMIRBodyContext,
    args: &[Option<HMIRExprID>],
    values: &mut [Option<HMIRValue>],
    from: usize,
    span: &TokenRange,
) -> EvalResult<()> {
    let function = deduction.function;
    let mut closures = Vec::new();
    for (index, arg) in args.iter().enumerate().skip(from) {
        let Some(arg) = arg.filter(|_| !deduction.is_open(index)) else {
            continue;
        };
        if let Some(closure) = closure(body, arg) {
            closures.push((index, closure));
        } else if let Some((actual, eta)) = try_actual_type(context, body, arg)? {
            let form = Form {
                argument: true,
                eta,
                ..Form::new(function.body(), *function.signature().params()[index].ty())
            };
            unify(deduction, context, frame, form, actual, span)?;
        }
    }
    bind(deduction, context, frame, body, values, span)?;
    for (index, (params, root)) in closures {
        infer_closure(
            deduction, context, frame, body, values, index, params, root, span,
        )?;
        bind(deduction, context, frame, body, values, span)?;
    }
    Ok(())
}

fn infer_closure(
    deduction: &mut Deduction,
    context: &mut LocalLoweringContext,
    frame: &mut Frame,
    body: &HMIRBodyContext,
    values: &mut [Option<HMIRValue>],
    index: usize,
    params: &[HMIRLocalID],
    root: HMIRExprID,
    span: &TokenRange,
) -> EvalResult<()> {
    let function = deduction.function;
    let form = *function.signature().params()[index].ty();
    let HMIRExprKind::Type(ty) = function.body().expr(form).kind() else {
        return Ok(());
    };
    let HMIRExprTypeKind::Function { signature, .. } = ty.kind() else {
        return Ok(());
    };
    if signature.params().len() != params.len() {
        return Ok(());
    }

    let quote = staged::closure(context, body, params, root)?;
    let declared = staged::declared(context, &quote, None)?;
    for (declared, param) in declared.iter().zip(signature.params()) {
        if let Some(ty) = *declared {
            let form = Form::new(function.body(), *param.ty());
            unify(deduction, context, frame, form, ty, span)?;
        }
    }
    bind(deduction, context, frame, body, values, span)?;

    let result_form = Form::new(function.body(), *signature.return_type());
    let references = staged::references(result_form.body, result_form.expr);
    if !deduction
        .slots
        .keys()
        .any(|&index| references.contains(&function.params()[index]))
    {
        return Ok(());
    }

    let mut types = Vec::with_capacity(params.len());
    for (declared, param) in declared.into_iter().zip(signature.params()) {
        let form = Form::new(function.body(), *param.ty());
        let ty = match declared {
            Some(ty) => ty,
            None => {
                let Some(ty) = try_probe(context, frame, form, span, eval_type)? else {
                    return Ok(());
                };
                ty
            }
        };
        types.push(ty);
    }

    let Some(result) = staged::synthesize(context, &quote, &types, span)?
        .standing()
        .copied()
    else {
        return Ok(());
    };
    let result = staged::lift(context.env_mut(), result, span);
    Ok(unify(deduction, context, frame, result_form, result, span)?)
}

pub(super) fn try_probe<T: Clone>(
    context: &mut LocalLoweringContext,
    frame: &mut Frame,
    form: Form<'_>,
    span: &TokenRange,
    evaluate: impl FnOnce(
        &mut LocalLoweringContext,
        &HMIRBodyContext,
        HMIRExprID,
    ) -> EvalResult<Evaluation<T>>,
) -> CHLResult<Option<T>> {
    let params = frame.params();
    frame.enter(context, span, |child| {
        let source = form.source.unwrap_or_else(|| child.source());
        let result = child.relocated(source, |child| evaluate(child, form.body, form.expr));
        match result {
            Ok(Evaluation::Ready(value)) => Ok(Some(value)),
            Ok(pending) => Ok(pending.standing().cloned()),
            Err(EvalExit::Diagnostic(error)) => {
                if error.message_as::<UnboundLocal>().is_some_and(|missing| {
                    missing.evaluation == child.evaluation() && params.contains(&missing.local)
                }) {
                    Ok(None)
                } else {
                    Err(error)
                }
            }
            Err(EvalExit::Stop(_)) => Err(hmir_error(
                form.body.expr(form.expr).span(),
                &mir::COMPTIME_CONTROL_ESCAPE,
                (),
            )),
        }
    })
}

fn closure(body: &HMIRBodyContext, mut arg: HMIRExprID) -> Option<(&[HMIRLocalID], HMIRExprID)> {
    while let HMIRExprKind::Piped(inner) = body.expr(arg).kind() {
        arg = *inner;
    }
    match body.expr(arg).kind() {
        HMIRExprKind::Quote { params, body } if !params.is_empty() => Some((params, *body)),
        _ => None,
    }
}

fn try_actual_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    mut arg: HMIRExprID,
) -> EvalResult<Option<(HMIRTypeID, bool)>> {
    let mut quoted = false;
    loop {
        match body.expr(arg).kind() {
            HMIRExprKind::Piped(inner) => {
                quoted = true;
                arg = *inner;
            }
            HMIRExprKind::Quote {
                params,
                body: inner,
            } if params.is_empty() => {
                quoted = true;
                arg = *inner;
            }
            HMIRExprKind::Hole(_) | HMIRExprKind::Quote { .. } => return Ok(None),
            _ => break,
        }
    }
    let mut eta = false;
    let span = body.expr(arg).span();
    let ty = subject::typed(context, span, false, |child| {
        let evaluation = eval(child, body, arg)?;
        eta = !quoted && evaluation.standing().is_some_and(|value| {
            let ty = child.env().referent(value.ty());
            types::runtime_function(child.env(), ty) && comptime::known(child, value).is_some()
        });
        Ok(evaluation)
    })?;
    Ok(ty.standing().map(|ty| (*ty, eta)))
}
