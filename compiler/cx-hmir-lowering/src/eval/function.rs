mod deduce;
mod specialize;

use std::mem;

use cx_hmir::{
    HMIRBody, HMIRDefID, HMIRDefKind, HMIRFnSignature, HMIRFunction, HMIRFunctionStage,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::function::HMIRFnParam,
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{ComptimeContext, FnContext, LocalLoweringContext},
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

use self::deduce::Deduction;

use super::{
    EvalResult, coerce, complete, comptime, contract, control, eval, eval_type,
    flow::{self, ComptimeStop, Evaluation, StopReason, ready},
    safety, staged, storage, types, unevaluated,
};

enum Argument<'a> {
    Bound(&'a HMIRValue),
    Expression(HMIRExprID),
}

struct Frame<'a> {
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
        let function = self.function;
        let runtime = self.runtime;
        self.enter(caller, span, |child| {
            let expr = *function.signature().params()[index].ty();
            let span = function.body().expr(expr).span();
            let ty = flow::top(eval_type(child, function.body(), expr)?.description())?;
            let ty = types::parameter(
                child,
                ty,
                function.signature().params()[index].comptime(),
                span,
            )?;
            if runtime && matches!(child.env().resolve_kind(ty), HMIRTypeKind::StagedExpr { .. })
            {
                return Err(hmir_error(
                    span,
                    &mir::COMPTIME_INVALID_OPERATION,
                    "a reference or staged-expression parameter".into(),
                ));
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
        if let HMIRValueKind::Constant(HMIRConstant::Type(ty)) = value.kind() {
            caller.prepare_type(*ty, arg_span)?;
        }
        let frozen = self.function.signature().params()[index].comptime();
        let value = if frozen {
            caller.env_mut().canonical_value(value)
        } else {
            value
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
            let target =
                emit::allocate(child, ty, declaration.name().cloned(), declaration.span())?;
            storage::initialize(child, &target, value, declaration.span())?;
            let reference = types::reference(child.env_mut(), ty, declaration.span());
            emit::storage::bind_local(child, local, target.with_ty(reference));
            Ok(())
        })
    }

    fn signature(
        &mut self,
        caller: &mut LocalLoweringContext,
        params: Vec<HMIRFnParam<HMIRTypeID>>,
        span: &TokenRange,
    ) -> CXResult<HMIRFnSignature<HMIRTypeID>> {
        let declared = self.function.signature();
        let function = self.function;
        let return_type = self.enter(caller, span, |child| {
            flow::top(eval_type(child, function.body(), *declared.return_type())?.description())
        })?;
        if self.runtime
            && matches!(caller.env().resolve_kind(return_type), HMIRTypeKind::StagedExpr { .. })
        {
            return Err(hmir_error(
                span,
                &mir::COMPTIME_INVALID_OPERATION,
                "a staged-expression result".into(),
            ));
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

    fn enter<T>(
        &mut self,
        caller: &mut LocalLoweringContext,
        span: &TokenRange,
        evaluate: impl FnOnce(&mut LocalLoweringContext) -> CXResult<T>,
    ) -> CXResult<T> {
        let mut child = caller
            .child(
                Some(self.function.signature()),
                FnContext::Comptime(mem::take(&mut self.state)),
                None,
            )
            .sourced(self.def.brand())
            .at(self.function.body().visible_through());
        child.enter_call(span)?;
        if let Some(ty) = self.return_type {
            child = child.with_return_type(ty);
        }
        let result = evaluate(&mut child);
        let FnContext::Comptime(state) = child.into_context() else {
            unreachable!()
        };
        self.state = state;
        result
    }
}

pub(crate) fn call(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    caller_body: &HMIRBody,
    args: &[HMIRExprID],
    applied: &[HMIRValue],
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Def(def))) = callee.kind()
    else {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_UNAVAILABLE,
            "this function".into(),
        ));
    };
    let unit = context
        .env_mut()
        .symbols_mut()
        .owner(def.brand())
        .expect("function owner exists");
    let source = unit.resolve_def(*def).expect("function exists");
    let selected = symbol::select(source, None)?;
    let Some(HMIRDefKind::Function(function)) = source.declarations()[selected].kind() else {
        unreachable!("a function constant names a function")
    };
    let declared = function.signature();
    let at_runtime = matches!(context.context(), FnContext::Runtime(_));
    let generator = at_runtime && declared.stage() == HMIRFunctionStage::Comptime;
    let runtime = at_runtime && !generator;
    if declared.is_variadic() {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_INVALID_OPERATION,
            "a variadic dependent function call".into(),
        ));
    }
    let mut bound = applied.iter();
    let mut supplied = args.iter();
    let arguments = declared
        .params()
        .iter()
        .map(|param| {
            if param.comptime()
                && let Some(value) = bound.next()
            {
                return Some(Argument::Bound(value));
            }
            supplied.next().map(|expr| Argument::Expression(*expr))
        })
        .collect::<Vec<_>>();
    let partial = arguments.iter().any(Option::is_none);
    let prefix = !args.is_empty()
        && declared
            .params()
            .iter()
            .zip(&arguments)
            .all(|(param, arg)| !matches!(arg, Some(Argument::Expression(_))) || param.comptime());
    if bound.next().is_some() || supplied.next().is_some() || (partial && !prefix) {
        return Err(hmir_error(
            span,
            &typecheck::ARGUMENT_COUNT,
            (
                format!("'{}'", source.name()),
                declared.params().len() - applied.len(),
                args.len(),
                false,
            ),
        ));
    }
    if !partial {
        safety::require_call(context, span, declared.contract().is_safe(), true)?;
    }
    let Some(_) = function.def() else {
        if runtime {
            return Err(hmir_error(
                span,
                &typecheck::UNSUPPORTED_FEATURE,
                "specializing a function without a definition".into(),
            ));
        }
        return Err(hmir_error(
            span,
            &mir::COMPTIME_NO_BODY,
            source.name().to_string(),
        ));
    };
    let mut frame = Frame {
        function,
        def: *def,
        state: ComptimeContext::new(),
        return_type: None,
        runtime,
    };
    let mut deduction = Deduction::new(function, source.name().to_string());
    let mut params: Vec<HMIRFnParam<HMIRTypeID>> = Vec::with_capacity(arguments.len());
    let mut values = vec![None; arguments.len()];
    for (index, (param, argument)) in declared.params().iter().zip(&arguments).enumerate() {
        let Some(argument) = argument else {
            break;
        };
        if let Argument::Bound(value) = argument {
            let ty = frame.parameter(context, index, span)?;
            params.push(HMIRFnParam::new(true, ty));
            values[index] =
                Some(frame.argument(context, index, ty, (*value).clone(), span, span)?);
            continue;
        }
        let Argument::Expression(arg) = argument else {
            unreachable!()
        };
        let frozen = param.comptime();
        let arg_span = caller_body.expr(*arg).span();
        let actual = match deduction.needs_actual() {
            true => actual_type(context, caller_body, *arg)?,
            false => None,
        };
        for (pending, argument, ty) in deduction.infer(context, index, actual, span)? {
            let value = ready!(argument.complete(context, caller_body, Some(ty)));
            let Some(Argument::Expression(expr)) = &arguments[pending] else {
                unreachable!()
            };
            let arg_span = caller_body.expr(*expr).span();
            let ty = *params[pending].ty();
            values[pending] = Some(frame.argument(context, pending, ty, value, span, arg_span)?);
        }
        let ty = frame.parameter(context, index, span)?;
        if !runtime && context.env().is_reference(ty) {
            return Err(hmir_error(
                arg_span,
                &mir::COMPTIME_INVALID_OPERATION,
                "a reference parameter".into(),
            ));
        }
        let staged = matches!(
            context.env().resolve_kind(ty),
            HMIRTypeKind::StagedExpr { .. }
        );
        let argument = if staged {
            staged::argument(context, caller_body, *arg, ty)?
        } else if (runtime && frozen) || generator {
            comptime::frozen(context, caller_body, *arg, Some(ty))?
        } else {
            let evaluated = eval(context, caller_body, *arg)?;
            let arg_span = arg_span.clone();
            complete::value(context, caller_body, evaluated, Some(ty))?.and_then(
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
        };
        params.push(HMIRFnParam::new(frozen, ty));
        match argument {
            Evaluation::Ready(value) => {
                values[index] = Some(frame.argument(context, index, ty, value, span, arg_span)?);
            }
            Evaluation::Incomplete(_)
                if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Type) =>
            {
                deduction.defer(index, argument);
            }
            Evaluation::Incomplete(incomplete) => return Err(incomplete.error()),
            Evaluation::Stop(stop) => return Ok(Evaluation::Stop(stop)),
        }
    }
    deduction.finish(span)?;
    if partial
        && (declared.stage() == HMIRFunctionStage::Comptime
            || declared.params()[params.len()..]
                .iter()
                .any(|param| param.comptime()))
    {
        let args = values.into_iter().flatten().collect();
        return Ok(Evaluation::Ready(HMIRValue::curried(
            callee.ty(),
            callee.clone(),
            args,
        )));
    }
    for index in params.len()..declared.params().len() {
        let ty = frame.parameter(context, index, span)?;
        params.push(HMIRFnParam::new(false, ty));
    }
    let signature = frame.signature(context, params, span)?;
    let returns_code = matches!(
        context.env().resolve_kind(*signature.return_type()),
        HMIRTypeKind::StagedExpr { .. }
    );
    if generator && !returns_code {
        let name = source.name().to_string();
        return Err(hmir_error(span, &mir::COMPTIME_FUNCTION_AT_RUNTIME, name));
    }
    for (index, declaration) in symbol::visible(source, context.visible_through()) {
        if index == selected {
            continue;
        }
        let Some(HMIRDefKind::Function(other)) = declaration.kind() else {
            continue;
        };
        let mut other = Frame {
            function: other,
            def: *def,
            state: ComptimeContext::new(),
            return_type: None,
            runtime,
        };
        if other.function.signature().params().len() != values.len() {
            return Err(hmir_error(
                declaration.span(),
                &typecheck::INCOMPATIBLE_DECLARATION,
                source.name().to_string(),
            ));
        }
        let mut params = Vec::with_capacity(values.len());
        for (index, (param, value)) in signature.params().iter().zip(&values).enumerate() {
            let ty = other.parameter(context, index, span)?;
            if !alike(context.env(), *param.ty(), ty, false) {
                return Err(hmir_error(
                    declaration.span(),
                    &typecheck::INCOMPATIBLE_DECLARATION,
                    source.name().to_string(),
                ));
            }
            let frozen = other.function.signature().params()[index].comptime();
            if frozen != param.comptime() {
                return Err(hmir_error(
                    declaration.span(),
                    &typecheck::INCOMPATIBLE_DECLARATION,
                    source.name().to_string(),
                ));
            }
            if (!runtime || frozen)
                && let Some(value) = value
            {
                other.bind(context, index, ty, value, span)?;
            }
            params.push(HMIRFnParam::new(frozen, ty));
        }
        let other = other.signature(context, params, span)?;
        if !signatures_alike(context.env(), &signature, &other) {
            return Err(hmir_error(
                declaration.span(),
                &typecheck::INCOMPATIBLE_DECLARATION,
                source.name().to_string(),
            ));
        }
    }
    if partial {
        return Ok(Evaluation::Ready(specialize::declare(
            context, *def, &signature, values, span,
        )?));
    }
    let values = values
        .into_iter()
        .map(|value| value.expect("all arguments are complete"))
        .collect::<Vec<_>>();
    if runtime {
        return specialize::apply(context, *def, &signature, values, span);
    }
    let value = frame.enter(context, span, |child| {
        execute(
            child,
            function,
            &signature,
            &values,
            &source.name().to_string(),
            span,
        )
    })?;
    match generator {
        true => staged::materialize(context, &value, &[]),
        false => Ok(Evaluation::Ready(value)),
    }
}

fn actual_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    arg: HMIRExprID,
) -> CXResult<Option<HMIRTypeID>> {
    let mut operand = arg;
    while let HMIRExprKind::Piped(inner) | HMIRExprKind::Quote { body: inner, .. } =
        body.expr(operand).kind()
    {
        operand = *inner;
    }
    if matches!(body.expr(operand).kind(), HMIRExprKind::Hole(_)) {
        return Ok(None);
    }
    Ok(Some(flow::top(unevaluated::subject(context, body, operand))?))
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
    let value = match body(context, function)? {
        Evaluation::Stop(StopReason::Comptime(ComptimeStop::Return(value))) => value,
        Evaluation::Stop(StopReason::Comptime(_)) => {
            return Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ()));
        }
        Evaluation::Incomplete(incomplete) => return Err(incomplete.error()),
        _ => unreachable!("a function body returns or diagnoses fallthrough"),
    };
    let value = flow::top(comptime::export(context, value, span))?;
    contract::check(
        context,
        signature.contract(),
        args,
        Some(&value),
        name,
        span,
    )?;
    Ok(value)
}

pub(crate) fn body(
    context: &mut LocalLoweringContext,
    function: &HMIRFunction,
) -> EvalResult<HMIRValue> {
    let definition = function.def().expect("an executed function has a body");
    let body = function.body();
    let root = body.expr(definition.root());
    if !matches!(root.kind(), HMIRExprKind::Block { .. }) {
        let value = eval(context, body, definition.root())?;
        return control::return_value(context, body, Some(value), false, root.span());
    }
    ready!(eval(context, body, definition.root()));
    let name = function.prototype().symbol_name();
    if matches!(context.context(), FnContext::Comptime(_)) {
        let ty = context
            .return_type()
            .expect("function return type is evaluated");
        match context.env().resolve_kind(ty) {
            HMIRTypeKind::Void => {
                return control::return_value(context, body, None, false, root.span());
            }
            HMIRTypeKind::Unreachable => {
                return Err(hmir_error(root.span(), &mir::COMPTIME_UNREACHABLE, ()));
            }
            HMIRTypeKind::Int(_) if name.as_str() == "main" => {}
            _ => {
                return Err(hmir_error(
                    root.span(),
                    &mir::FUNCTION_RETURN,
                    name.to_string(),
                ));
            }
        }
    }
    control::fallthrough(context, body, name.as_str() == "main", root.span())
}
