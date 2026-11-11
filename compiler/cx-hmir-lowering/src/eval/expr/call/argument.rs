use cx_hmir::{
    HMIRBodyContext, HMIRDefID, HMIRDefKind, HMIRFnSignature,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::catalogue::mir;
use cx_tokens::TokenRange;

use crate::{
    env::function::{ComptimeContext, LocalLoweringContext},
    eval::{
        EvalResult, coerce, comptime,
        deduce::{self, Deduction},
        eval,
        flow::Evaluation,
        staged,
    },
    log::hmir_error,
    symbol,
    value::{self, HMIRValue},
};

use super::{
    bind::{Binding, Slot},
    dependent::{self, Frame},
};

// The value of each parameter of 'callee' that 'binding' gives one, and then of each variadic argument
pub(crate) fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    callee: &HMIRValue,
    signature: &HMIRFnSignature<HMIRTypeID>,
    binding: Binding,
    span: &TokenRange,
) -> EvalResult<Evaluation<Vec<Option<HMIRValue>>>> {
    let referent = context.env().referent(callee.ty());
    match value::as_function(callee).filter(|_| dependent::applies(context, referent)) {
        Some(def) => applied(context, body, def, binding, span),
        None => emitted(context, body, signature, binding).map(Evaluation::Ready),
    }
}

fn emitted(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    signature: &HMIRFnSignature<HMIRTypeID>,
    binding: Binding,
) -> EvalResult<Vec<Option<HMIRValue>>> {
    let mut values = Vec::with_capacity(binding.slots.len() + binding.variadic.len());
    for (slot, param) in binding.slots.into_iter().zip(signature.params()) {
        values.push(match slot {
            Slot::Bound(value) => Some(value),
            Slot::Argument(arg) => {
                let value = eval(context, body, arg)?.complete(context, body, Some(*param.ty()))?;
                let span = body.expr(arg).span();
                Some(coerce::implicit::standard(context, value, *param.ty(), span)?)
            }
            Slot::Default | Slot::Open => None,
        });
    }
    for arg in binding.variadic {
        let value = eval(context, body, arg)?.complete(context, body, None)?;
        values.push(Some(coerce::default_argument(context, value, body.expr(arg).span())?));
    }
    Ok(values)
}

// The arguments of a function that is applied to them: each is evaluated as the type of its parameter has it,
// which the arguments of the comptime parameters before it decide
fn applied(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    def: HMIRDefID,
    binding: Binding,
    span: &TokenRange,
) -> EvalResult<Evaluation<Vec<Option<HMIRValue>>>> {
    let unit = context
        .env_mut()
        .symbols_mut()
        .owner(def.brand())
        .expect("function owner exists");
    let source = unit.resolve_def(def).expect("function exists");
    let name = source.name().to_string();
    let selected = symbol::select(source, None)?;
    let Some(HMIRDefKind::Function(function)) = source.declarations()[selected].kind() else {
        unreachable!("a function constant names a function")
    };
    if function.signature().is_variadic() {
        let operation = "a variadic dependent function call".into();
        return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, operation).into());
    }
    let spliced = dependent::is_spliced(context, function);
    let runtime = context.is_runtime() && !spliced;
    let partial = binding.is_partial();

    let mut frame = Frame::new(function, def, ComptimeContext::of(name.clone()), runtime);
    let mut deduction = Deduction::new(function, name);
    let mut deferred = vec![None; binding.slots.len()];
    let mut values = vec![None; binding.slots.len()];
    for (index, slot) in binding.slots.iter().enumerate() {
        let arg = match slot {
            Slot::Open => continue,
            Slot::Bound(value) => {
                let ty = frame.parameter(context, index, span)?;
                values[index] = Some(frame.argument(context, index, ty, value.clone(), span)?);
                continue;
            }
            Slot::Argument(arg) => Some(*arg),
            Slot::Default => None,
        };
        let arg_span = arg.map_or(span, |arg| body.expr(arg).span());
        let actual = match arg.filter(|_| deduction.needs_actual()) {
            Some(arg) => deduce::actual_type(context, body, arg)?,
            None => None,
        };
        for deduced in deduction.infer(context, &mut frame, index, actual, span)? {
            let value = deduced.argument.complete(context, body, Some(deduced.ty))?;
            let (ty, arg_span): (HMIRTypeID, &TokenRange) =
                deferred[deduced.index].expect("a deferred argument has its parameter");
            let value = frame.argument(context, deduced.index, ty, value, arg_span)?;
            values[deduced.index] = Some(value);
        }

        let ty = frame.parameter(context, index, span)?;
        if !runtime && context.env().is_reference(ty) {
            let operation = "a reference parameter".into();
            return Err(hmir_error(arg_span, &mir::COMPTIME_INVALID_OPERATION, operation).into());
        }
        let frozen = function.signature().params()[index].comptime();
        let argument = match arg {
            Some(arg) if staged::is_code(context, ty) => staged::argument(context, body, arg, ty)?,
            Some(arg) if (runtime && frozen) || spliced => {
                comptime::freeze(context, body, arg, Some(ty))?
            }
            Some(arg) => {
                let evaluated = eval(context, body, arg)?;
                deduce::complete(context, body, evaluated, Some(ty))?
            }
            None => deduce::hole(context, span)?,
        };
        match argument {
            Evaluation::Ready(value) => {
                values[index] = Some(frame.argument(context, index, ty, value, arg_span)?);
            }
            Evaluation::Incomplete(_)
                if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Type) =>
            {
                deferred[index] = Some((ty, arg_span));
                deduction.defer(index, argument);
            }
            Evaluation::Incomplete(incomplete) => return Err(incomplete.error().into()),
        }
    }
    match deduction.finish(span) {
        Ok(()) => Ok(Evaluation::Ready(values)),
        Err(undeduced) if !partial && deduce::generates_type(function) => {
            Ok(deduce::generated(context, def, values, undeduced, span))
        }
        Err(undeduced) => Err(undeduced.into()),
    }
}
