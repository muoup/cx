use std::mem;

use cx_hmir::{
    HMIRBody, HMIRDefID, HMIRDefKind, HMIRFnSignature, HMIRFunction, HMIRFunctionStage,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::function::HMIRFnParam,
};
use cx_log::{
    CXResult,
    catalogue::{analysis, mir, typecheck},
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit,
    env::function::{ComptimeContext, FnContext, LocalLoweringContext},
    eval::{
        EvalResult, coerce, comptime, control,
        deduce::{self, Deduction},
        eval, eval_type,
        flow::{self, ComptimeStop, EvalExit, Evaluation, StopReason},
        staged, types,
    },
    log::hmir_error,
    symbol::{
        self,
        composite::{alike, signatures_alike},
    },
    value::{
        HMIRValue, HMIRValueKind,
        constant::{HMIRConstant, HMIRFunctionRef},
    },
};

use super::{contract, specialize};

enum Argument<'a> {
    Bound(&'a HMIRValue),
    Expression(HMIRExprID),
    Default,
}

pub(crate) fn parameter_name(function: &HMIRFunction, index: usize) -> Option<&CXIdent> {
    function.body().local(function.params()[index]).name()
}

// Binds the parameters of 'function' from its curried values, its named arguments and then, in order, the
// positional arguments to the parameters left that have no default. Unless the call is partial, a parameter
// left with a default is a hole.
fn bind<'a>(
    function: &HMIRFunction,
    name: &str,
    caller_body: &HMIRBody,
    applied: &'a [Option<HMIRValue>],
    args: &[HMIRExprID],
    named: &[(CXIdent, HMIRExprID)],
    span: &TokenRange,
) -> CXResult<Vec<Option<Argument<'a>>>> {
    // REVIEW: This method is a little dense
    let declared = function.signature();
    let comptime = |param: &HMIRFnParam<HMIRExprID>| {
        param.comptime() || declared.stage() == HMIRFunctionStage::Comptime
    };
    let mut arguments = (0..declared.params().len())
        .map(|index| applied.get(index).and_then(Option::as_ref).map(Argument::Bound))
        .collect::<Vec<_>>();
    for (argument, expr) in named {
        let arg_span = caller_body.expr(*expr).span();
        let Some(index) = (0..arguments.len()).find(|index| {
            comptime(&declared.params()[*index]) && parameter_name(function, *index) == Some(argument)
        }) else {
            let subject = (format!("'{name}'"), argument.to_string());
            return Err(hmir_error(arg_span, &typecheck::UNKNOWN_PARAMETER, subject));
        };
        if arguments[index].is_some() {
            let subject = (format!("argument '{argument}'"), format!("call to '{name}'"));
            return Err(hmir_error(arg_span, &typecheck::DUPLICATE_ITEM, subject));
        }
        arguments[index] = Some(Argument::Expression(*expr));
    }
    let positional = declared
        .params()
        .iter()
        .enumerate()
        .filter(|(index, param)| arguments[*index].is_none() && !param.defaulted())
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    if args.len() > positional.len() {
        let counts = (format!("'{name}'"), positional.len(), args.len(), false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, counts));
    }
    for (index, expr) in positional.iter().zip(args) {
        arguments[*index] = Some(Argument::Expression(*expr));
    }

    let partial = positional.len() > args.len();
    let supplied = !args.is_empty() || !named.is_empty();
    let runtime_supplied = declared
        .params()
        .iter()
        .zip(&arguments)
        .any(|(param, arg)| !param.comptime() && matches!(arg, Some(Argument::Expression(_))));
    if partial && (!supplied || runtime_supplied) {
        let counts = (format!("'{name}'"), positional.len(), args.len(), false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, counts));
    }
    if !partial {
        for (param, argument) in declared.params().iter().zip(&mut arguments) {
            if param.defaulted() && argument.is_none() {
                *argument = Some(Argument::Default);
            }
        }
    }
    Ok(arguments)
}

pub(crate) struct Frame<'a> {
    function: &'a HMIRFunction,
    def: HMIRDefID,
    state: ComptimeContext,
    return_type: Option<HMIRTypeID>,
    runtime: bool,
}

impl Frame<'_> {
    fn parameter(
        &mut self,
        caller: &mut LocalLoweringContext,
        index: usize,
        span: &TokenRange,
    ) -> CXResult<HMIRTypeID> {
        let (function, runtime) = (self.function, self.runtime);
        self.enter(caller, span, |child| {
            let param = &function.signature().params()[index];
            let span = function.body().expr(*param.ty()).span();
            let ty = flow::top(eval_type(child, function.body(), *param.ty()).and_then(|ty| ty.description()))?;
            let ty = types::parameter(child, ty, param.comptime(), span)?;
            if runtime && staged::is_code(child, ty) {
                let operation = "a reference or staged-expression parameter".into();
                return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, operation).into());
            }
            Ok(ty)
        })
    }

    fn argument(
        &mut self,
        caller: &mut LocalLoweringContext,
        index: usize,
        ty: HMIRTypeID,
        value: HMIRValue,
        span: &TokenRange,
        arg_span: &TokenRange,
    ) -> CXResult<HMIRValue> {
        if let Some(ty) = value.as_type() {
            caller.prepare_type(ty, arg_span)?;
        }
        let frozen = self.function.signature().params()[index].comptime();
        let value = match frozen {
            true => caller.env_mut().canonical_value(value),
            false => value,
        };
        if !self.runtime || frozen {
            self.bind(caller, index, ty, &value, span)?;
        }
        Ok(value)
    }

    fn bind(
        &mut self,
        caller: &mut LocalLoweringContext,
        index: usize,
        ty: HMIRTypeID,
        value: &HMIRValue,
        span: &TokenRange,
    ) -> CXResult<()> {
        let function = self.function;
        self.enter(caller, span, |child| {
            let local = function.params()[index];
            let declaration = function.body().local(local);
            let span = declaration.span();
            let target = emit::allocate(child, ty, declaration.name().cloned(), span)?;
            emit::initialize(child, &target, Some(value), span)?;
            let reference = types::reference(child.env_mut(), ty, None, span);
            emit::bind_local(child, local, target.with_ty(reference));
            Ok(())
        })
    }

    fn signature(
        &mut self,
        caller: &mut LocalLoweringContext,
        params: Vec<HMIRFnParam<HMIRTypeID>>,
        span: &TokenRange,
    ) -> CXResult<HMIRFnSignature<HMIRTypeID>> {
        let function = self.function;
        let declared = function.signature();
        let return_type = self.enter(caller, span, |child| {
            flow::top(eval_type(child, function.body(), *declared.return_type()).and_then(|ty| ty.description()))
        })?;
        if self.runtime && staged::is_code(caller, return_type) {
            let operation = "a staged-expression result".into();
            return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, operation).into());
        }
        self.return_type = Some(return_type);
        Ok(HMIRFnSignature::new(
            declared.stage(),
            params,
            return_type,
            declared.is_variadic(),
            declared.contract().clone(),
        ))
    }

    pub(crate) fn enter<T>(
        &mut self,
        caller: &mut LocalLoweringContext,
        span: &TokenRange,
        evaluate: impl FnOnce(&mut LocalLoweringContext) -> CXResult<T>,
    ) -> CXResult<T> {
        let state = FnContext::Comptime(mem::take(&mut self.state));
        let mut child = caller
            .child(Some(self.function.signature()), state, None)
            .sourced(self.def.brand())
            .at(self.function.body().visible_through());
        child.enter_call(span)?;
        if let Some(ty) = self.return_type {
            child = child.with_return_type(ty);
        }
        let result = evaluate(&mut child);
        self.state = child.into_comptime();
        result
    }
}

pub(crate) fn dependent(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    caller_body: &HMIRBody,
    args: &[HMIRExprID],
    named: &[(CXIdent, HMIRExprID)],
    applied: &[Option<HMIRValue>],
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Def(def))) = callee.kind()
    else {
        return Err(hmir_error(span, &mir::COMPTIME_UNAVAILABLE, "this function".into()).into());
    };
    let unit = context.env_mut().symbols_mut().owner(def.brand()).expect("function owner exists");
    let source = unit.resolve_def(*def).expect("function exists");
    let name = source.name().to_string();
    let selected = symbol::select(source, None)?;
    let Some(HMIRDefKind::Function(function)) = source.declarations()[selected].kind() else {
        unreachable!("a function constant names a function")
    };
    let declared = function.signature();
    let spliced = context.is_runtime() && declared.stage() == HMIRFunctionStage::Comptime;
    let runtime = context.is_runtime() && !spliced;
    if declared.is_variadic() {
        let operation = "a variadic dependent function call".into();
        return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, operation).into());
    }
    let arguments = bind(function, &name, caller_body, applied, args, named, span)?;
    let partial = arguments.iter().any(Option::is_none);
    if !partial {
        super::require(context, span, declared.contract().is_safe(), true)?;
    }
    if function.def().is_none() {
        return Err(match runtime {
            true => {
                let feature = "specializing a function without a definition".into();
                hmir_error(span, &typecheck::UNSUPPORTED_FEATURE, feature)
            }
            false => hmir_error(span, &mir::COMPTIME_NO_BODY, name),
        }.into());
    }
    let mut frame = Frame {
        function,
        def: *def,
        state: ComptimeContext::of(name.clone()),
        return_type: None,
        runtime,
    };
    let mut deduction = Deduction::new(function, name.clone());
    let mut params: Vec<Option<HMIRFnParam<HMIRTypeID>>> = vec![None; arguments.len()];
    let mut values = vec![None; arguments.len()];
    for (index, (param, argument)) in declared.params().iter().zip(&arguments).enumerate() {
        let arg = match argument {
            None => continue,
            Some(Argument::Bound(value)) => {
                let ty = frame.parameter(context, index, span)?;
                params[index] = Some(HMIRFnParam::new(true, ty, param.defaulted()));
                values[index] = Some(frame.argument(context, index, ty, (*value).clone(), span, span)?);
                continue;
            }
            Some(Argument::Expression(arg)) => Some(*arg),
            Some(Argument::Default) => None,
        };
        let frozen = param.comptime();
        let arg_span = arg.map_or(span, |arg| caller_body.expr(arg).span());
        let actual = match (arg, deduction.needs_actual()) {
            (Some(arg), true) => deduce::actual_type(context, caller_body, arg)?,
            _ => None,
        };
        for deduced in deduction.infer(context, &mut frame, index, actual, span)? {
            let value = deduced.argument.complete(context, caller_body, Some(deduced.ty))?;
            let ty = *params[deduced.index].as_ref().expect("a deferred argument has its parameter").ty();
            let arg_span = match &arguments[deduced.index] {
                Some(Argument::Expression(expr)) => caller_body.expr(*expr).span(),
                _ => span,
            };
            values[deduced.index] = Some(frame.argument(context, deduced.index, ty, value, span, arg_span)?);
        }
        let ty = frame.parameter(context, index, span)?;
        if !runtime && context.env().is_reference(ty) {
            let operation = "a reference parameter".into();
            return Err(hmir_error(arg_span, &mir::COMPTIME_INVALID_OPERATION, operation).into());
        }
        let argument = if let Some(arg) = arg.filter(|_| staged::is_code(context, ty)) {
            staged::argument(context, caller_body, arg, ty)?
        } else if let Some(arg) = arg.filter(|_| (runtime && frozen) || spliced) {
            comptime::freeze(context, caller_body, arg, Some(ty))?
        } else if let Some(arg) = arg {
            let evaluated = eval(context, caller_body, arg)?;
            let arg_span = arg_span.clone();
            deduce::complete(context, caller_body, evaluated, Some(ty))?.and_then(
                context,
                caller_body,
                None,
                move |context, _, value, _| {
                    let value = coerce::implicit(context, value, ty, &arg_span)?;
                    match runtime {
                        true => Ok(Evaluation::Ready(value)),
                        false => comptime::export(context, value, &arg_span),
                    }
                },
            )?
        } else {
            deduce::hole(context, span)?
        };
        params[index] = Some(HMIRFnParam::new(frozen, ty, param.defaulted()));
        match argument {
            Evaluation::Ready(value) => {
                values[index] = Some(frame.argument(context, index, ty, value, span, arg_span)?);
            }
            Evaluation::Incomplete(_)
                if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Type) =>
            {
                deduction.defer(index, argument);
            }
            Evaluation::Incomplete(incomplete) => return Err(incomplete.error().into()),
        }
    }
    if let Err(undeduced) = deduction.finish(span) {
        return match !partial && deduce::generates_type(function) {
            true => Ok(deduce::generated(context, callee, values, undeduced, span)),
            false => Err(undeduced.into()),
        };
    }
    let curries = declared.stage() == HMIRFunctionStage::Comptime
        || declared.params().iter().zip(&params).any(|(param, bound)| bound.is_none() && param.comptime());
    if partial && curries {
        return Ok(Evaluation::Ready(HMIRValue::curried(callee.ty(), callee.clone(), values)));
    }
    let mut evaluated = Vec::with_capacity(params.len());
    for (index, param) in params.into_iter().enumerate() {
        evaluated.push(match param {
            Some(param) => param,
            None => HMIRFnParam::new(false, frame.parameter(context, index, span)?, false),
        });
    }
    let signature = frame.signature(context, evaluated, span)?;
    for (index, declaration) in symbol::visible(source, context.visible_through()) {
        let Some(HMIRDefKind::Function(other)) = declaration.kind() else {
            continue;
        };
        if index == selected {
            continue;
        }
        let incompatible = || hmir_error(declaration.span(), &typecheck::INCOMPATIBLE_DECLARATION, name.clone());
        let mut other = Frame {
            function: other,
            def: *def,
            state: ComptimeContext::new(),
            return_type: None,
            runtime,
        };
        if other.function.signature().params().len() != values.len() {
            return Err(incompatible().into());
        }
        let mut params = Vec::with_capacity(values.len());
        for (index, (param, value)) in signature.params().iter().zip(&values).enumerate() {
            let ty = other.parameter(context, index, span)?;
            let declared = &other.function.signature().params()[index];
            let frozen = declared.comptime();
            if !alike(context.env(), *param.ty(), ty, false)
                || frozen != param.comptime()
                || declared.defaulted() != param.defaulted()
            {
                return Err(incompatible().into());
            }
            if (!runtime || frozen)
                && let Some(value) = value
            {
                other.bind(context, index, ty, value, span)?;
            }
            params.push(HMIRFnParam::new(frozen, ty, declared.defaulted()));
        }
        let other = other.signature(context, params, span)?;
        if !signatures_alike(context.env(), &signature, &other) {
            return Err(incompatible().into());
        }
    }
    if partial {
        let value = specialize::declare(context, *def, &signature, values, span)?;
        return Ok(Evaluation::Ready(value));
    }
    let values = values
        .into_iter()
        .map(|value| value.expect("all arguments are complete"))
        .collect::<Vec<_>>();
    if runtime {
        return specialize::apply(context, *def, &signature, values, span);
    }
    let value = frame.enter(context, span, |child| {
        execute(child, function, &signature, &values, &name, span)
    })?;
    match spliced {
        true => staged::splice(context, value, span),
        false => Ok(Evaluation::Ready(value)),
    }
}

fn execute(
    context: &mut LocalLoweringContext,
    function: &HMIRFunction,
    signature: &HMIRFnSignature<HMIRTypeID>,
    args: &[HMIRValue],
    name: &str,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    contract::check(context, signature.contract(), args, None, name, span)?;
    let value = match body(context, function) {
        Err(EvalExit::Stop(StopReason::Comptime(ComptimeStop::Return(value)))) => value,
        Err(EvalExit::Stop(StopReason::Comptime(_))) => {
            return Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ()).into());
        }
        Err(EvalExit::Stop(StopReason::Runtime(stop))) => unreachable!("runtime stop {stop:?} in a comptime function"),
        Err(EvalExit::Diagnostic(error)) => return Err(error),
        Ok(Evaluation::Incomplete(incomplete)) => return Err(incomplete.error().into()),
        Ok(Evaluation::Ready(_)) => unreachable!("a function body returns or diagnoses fallthrough"),
    };
    let value = flow::top(comptime::export(context, value, span))?;
    let env = context.env();
    if let Some(slot) = context.comptime().unconsumed(|ty| env.is_nodrop(ty)) {
        let place = slot.name().map_or_else(|| "temporary".into(), |place| place.to_string());
        let discarded = place == "_";
        let args = (name.to_string(), "variable".into(), place, "function".into(), discarded);
        return Err(hmir_error(span, &analysis::VALUE_NOT_CONSUMED, args).into());
    }
    contract::check(context, signature.contract(), args, Some(&value), name, span)?;
    Ok(value)
}

pub(crate) fn body(
    context: &mut LocalLoweringContext,
    function: &HMIRFunction,
) -> EvalResult<Evaluation<HMIRValue>> {
    let definition = function.def().expect("an executed function has a body");
    let body = function.body();
    let root = body.expr(definition.root());
    if !matches!(root.kind(), HMIRExprKind::Block { yields: false, .. }) {
        let value = eval(context, body, definition.root())?;
        return control::return_value(context, body, Some(value), false, root.span());
    }
    (eval(context, body, definition.root()))?.ready()?;
    let name = function.prototype().symbol_name();
    if !context.is_runtime() {
        let ty = context.return_type().expect("function return type is evaluated");
        match context.env().resolve_kind(ty) {
            HMIRTypeKind::Void => {
                return control::return_value(context, body, None, false, root.span());
            }
            HMIRTypeKind::Unreachable => {
                return Err(hmir_error(root.span(), &mir::COMPTIME_UNREACHABLE, ()).into());
            }
            HMIRTypeKind::Int(_) if name.as_str() == "main" => {}
            _ => return Err(hmir_error(root.span(), &mir::FUNCTION_RETURN, name.to_string()).into()),
        }
    }
    control::fallthrough(context, body, name.as_str() == "main", root.span())
}
