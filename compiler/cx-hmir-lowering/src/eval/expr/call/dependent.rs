use std::mem;

use cx_hmir::{
    HMIRBodyContext, HMIRDefID, HMIRFnSignature, HMIRFunction, HMIRFunctionStage,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::function::HMIRFnParam,
};
use cx_log::{
    CXResult,
    catalogue::{analysis, mir, typecheck},
};
use cx_mir::MIRFunctionID;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
        generation::Generation,
        resolver::demand,
    },
    eval::{
        EvalResult, coerce, comptime, control,
        deduce::{self, Deduction},
        eval, eval_type,
        flow::{self, ComptimeStop, EvalExit, Evaluation, StopReason},
        staged, symbol, types,
    },
    log::hmir_error,
    lower::{self, declaration},
    value::{self, HMIRValue, immediate::HMIRImmediate},
};

use super::contract;

// The comptime state of a function as it is called. The arguments of its comptime parameters are bound in it as
// they arrive, so that the types of the parameters after them can be evaluated
pub(crate) struct Frame<'a> {
    function: &'a HMIRFunction,
    def: HMIRDefID,
    state: ComptimeContext,
    types: Vec<Option<HMIRTypeID>>,
    return_type: Option<HMIRTypeID>,
    runtime: bool,
}

impl<'a> Frame<'a> {
    // 'runtime' is whether the call is emitted rather than executed here, which leaves the arguments of the
    // parameters that are not comptime unbound
    pub(crate) fn new(
        function: &'a HMIRFunction,
        def: HMIRDefID,
        state: ComptimeContext,
        runtime: bool,
    ) -> Self {
        Self {
            function,
            def,
            state,
            types: vec![None; function.params().len()],
            return_type: None,
            runtime,
        }
    }

    pub(crate) fn parameter(
        &mut self,
        caller: &mut LocalLoweringContext,
        index: usize,
        span: &TokenRange,
    ) -> CXResult<HMIRTypeID> {
        if let Some(ty) = self.types[index] {
            return Ok(ty);
        }
        let (function, runtime) = (self.function, self.runtime);
        let ty = self.enter(caller, span, |child| {
            let param = &function.signature().params()[index];
            let span = function.body().expr(*param.ty()).span();
            let ty = flow::top(
                eval_type(child, function.body(), *param.ty()).and_then(|ty| ty.description()),
            )?;
            let ty = types::parameter(child, ty, param.comptime(), span)?;
            if runtime && staged::is_code(child, ty) {
                let operation = "a reference or staged-expression parameter".into();
                return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, operation));
            }
            Ok(ty)
        })?;
        self.types[index] = Some(ty);
        Ok(ty)
    }

    // 'value' as the argument of the parameter 'index'
    pub(crate) fn argument(
        &mut self,
        caller: &mut LocalLoweringContext,
        index: usize,
        value: HMIRValue,
        span: &TokenRange,
    ) -> CXResult<HMIRValue> {
        let ty = self.parameter(caller, index, span)?;
        if let Some(ty) = value::as_type(&value) {
            demand::prepare(caller.env_mut(), ty)?;
        }
        let value = coerce::implicit::standard(caller, value, ty, span)?;
        let frozen = self.function.signature().params()[index].comptime();
        if self.runtime && !frozen {
            return Ok(value);
        }
        let value = match caller.is_runtime() {
            true => value,
            false => comptime::converge(caller, value, span)?,
        };
        let function = self.function;
        self.enter(caller, span, |child| {
            let local = function.params()[index];
            let declaration = function.body().local(local);
            let span = declaration.span();
            let target = emit::storage::allocate(child, ty, declaration.name().cloned(), span)?;
            emit::storage::initialize(child, &target, Some(&value), span)?;
            emit::storage::bind_local(child, local, target);
            Ok(())
        })?;
        Ok(value)
    }

    // Each of 'values' as the argument of its parameter. A parameter without a value must not be one that the
    // types of the parameters after it depend on
    fn instantiate(
        &mut self,
        caller: &mut LocalLoweringContext,
        values: Vec<Option<HMIRValue>>,
        span: &TokenRange,
    ) -> CXResult<Vec<Option<HMIRValue>>> {
        let mut arguments = Vec::with_capacity(values.len());
        for (index, value) in values.into_iter().enumerate() {
            arguments.push(match value {
                Some(value) => Some(self.argument(caller, index, value, span)?),
                None => None,
            });
        }
        Ok(arguments)
    }

    // The signature that the function has for the arguments bound so far
    fn signature(
        &mut self,
        caller: &mut LocalLoweringContext,
        span: &TokenRange,
    ) -> CXResult<HMIRFnSignature<HMIRTypeID>> {
        let function = self.function;
        let declared = function.signature();
        let mut params = Vec::with_capacity(declared.params().len());
        for (index, param) in declared.params().iter().enumerate() {
            let ty = self.parameter(caller, index, span)?;
            params.push(HMIRFnParam::new(param.comptime(), ty));
        }
        let return_type = self.enter(caller, span, |child| {
            flow::top(
                eval_type(child, function.body(), *declared.return_type())
                    .and_then(|ty| ty.description()),
            )
        })?;
        if self.runtime && staged::is_code(caller, return_type) {
            let operation = "a staged-expression result".into();
            return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, operation));
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
            .sourced(self.def.brand());
        child.enter_call(span)?;
        if let Some(ty) = self.return_type {
            child = child.with_return_type(ty);
        }
        let result = evaluate(&mut child);
        self.state = child.into_comptime();
        result
    }
}

// Whether a call to a function of type 'ty' is applied to its arguments here, rather than emitted as it is
pub(crate) fn applies(context: &LocalLoweringContext, ty: HMIRTypeID) -> bool {
    !context.is_runtime() || types::comptime_only(context.env(), ty)
}

// Whether a call to 'function' executes it here for the code that it produces, which is emitted in its place
fn is_spliced(context: &LocalLoweringContext, function: &HMIRFunction) -> bool {
    context.is_runtime() && function.signature().stage() == HMIRFunctionStage::Comptime
}

// Calls the function 'def' with 'args', each evaluated as the type of its parameter has it, which the arguments
// of the comptime parameters before it decide. 'applied' are the values that a curried call has bound. A
// parameter with neither takes its default, unless the call is partial, which leaves it open
pub(crate) fn call(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    def: HMIRDefID,
    function: &HMIRFunction,
    mut applied: Vec<Option<HMIRValue>>,
    args: &[Option<HMIRExprID>],
    partial: bool,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let params = function.signature().params();
    if function.signature().is_variadic() {
        let operation = "a variadic dependent function call".into();
        return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, operation).into());
    }
    let name = name(context, def);
    let spliced = is_spliced(context, function);
    let runtime = context.is_runtime() && !spliced;
    let mut frame = Frame::new(function, def, ComptimeContext::of(name.clone()), runtime);

    let prototype = function.prototype();
    let default = |index: usize| prototype.params()[index].default().filter(|_| !partial);
    let is_hole = |index: &usize| {
        let hole = |body: &HMIRBodyContext, expr| matches!(body.expr(expr).kind(), HMIRExprKind::Hole(_));
        match (args[*index], default(*index)) {
            _ if applied.get(*index).is_some_and(Option::is_some) => false,
            (Some(arg), _) => hole(body, arg),
            (None, Some(default)) => hole(prototype.defaults(), default),
            (None, None) => false,
        }
    };
    let holes = (0..params.len()).filter(is_hole).collect();
    let deduction = Deduction::infer(context, &mut frame, function, name, holes, body, args, span)?;

    let mut values = vec![None; params.len()];
    let mut undeduced = None;
    for index in 0..params.len() {
        if let Some(value) = applied.get_mut(index).and_then(Option::take) {
            values[index] = Some(frame.argument(context, index, value, span)?);
            continue;
        }
        let arg = args[index];
        if arg.is_none() && default(index).is_none() {
            continue;
        }
        let arg_span = arg.map_or(span, |arg| body.expr(arg).span());
        let ty = frame.parameter(context, index, span)?;
        if !runtime && context.env().is_reference(ty) {
            let operation = "a reference parameter".into();
            return Err(hmir_error(arg_span, &mir::COMPTIME_INVALID_OPERATION, operation).into());
        }
        let value = match arg {
            _ if deduction.is_hole(index) => match deduction.inferred(index) {
                Some(inferred) => {
                    let universe = types::universe(context.env_mut(), arg_span);
                    HMIRValue::new(universe, HMIRImmediate::Type(inferred))
                }
                None if !partial && deduce::generates_type(function) => {
                    undeduced.get_or_insert(index);
                    continue;
                }
                None => return Err(deduction.undeduced(index, span).into()),
            },
            Some(arg) if staged::is_code(context, ty) => {
                staged::argument(context, body, arg, ty)?.ready()?
            }
            Some(arg) if (runtime && params[index].comptime()) || spliced => {
                comptime::freeze(context, body, arg, Some(ty))?.ready()?
            }
            Some(arg) => {
                let evaluated = eval(context, body, arg)?;
                deduce::complete(context, body, evaluated, Some(ty))?.ready()?
            }
            None => super::default(context, def, function, index, ty)?,
        };
        values[index] = Some(frame.argument(context, index, value, arg_span)?);
    }

    let Some(index) = undeduced else {
        return finish(context, frame, values, span);
    };
    let span = span.clone();
    let undeduced = deduction.undeduced(index, &span);
    deduce::generated(context, def, values, undeduced, &span).and_then(
        context,
        body,
        move |context, _, values| apply(context, def, values, &span),
    )
}

// Applies the function 'def' to the values given for its parameters; one without a value is left open
pub(crate) fn apply(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    values: Vec<Option<HMIRValue>>,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let unit = context
        .env_mut()
        .symbols_mut()
        .owner(def.brand())
        .expect("function owner exists");
    let function = declaration::function(unit.resolve_def(def).expect("function exists"));
    let runtime = context.is_runtime() && !is_spliced(context, function);
    let state = ComptimeContext::of(name(context, def));
    let mut frame = Frame::new(function, def, state, runtime);
    let values = frame.instantiate(context, values, span)?;
    finish(context, frame, values, span)
}

// Given a value for every parameter, a call that is emitted calls the function generated for its comptime
// arguments, and any other call executes the function here. Short of them all, the result is the function
// curried with those given
fn finish(
    context: &mut LocalLoweringContext,
    mut frame: Frame,
    values: Vec<Option<HMIRValue>>,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let (function, def) = (frame.function, frame.def);
    let declared = function.signature();
    let open = declared.params().iter().zip(&values);
    let staged = declared.stage() == HMIRFunctionStage::Comptime;
    if open.clone().any(|(param, value)| value.is_none() && (staged || param.comptime())) {
        let signature = symbol::signature(context.env_mut(), def)?;
        return Ok(Evaluation::Ready(curried(context, def, &signature, values, span)?));
    }

    let name = name(context, def);
    let spliced = is_spliced(context, function);
    if function.def().is_none() {
        return Err(match frame.runtime {
            true => {
                let feature = "specializing a function without a definition".into();
                hmir_error(span, &typecheck::UNSUPPORTED_FEATURE, feature)
            }
            false => hmir_error(span, &mir::COMPTIME_NO_BODY, name),
        }
        .into());
    }
    let signature = frame.signature(context, span)?;
    let Some(args) = values.iter().cloned().collect::<Option<Vec<_>>>() else {
        return Ok(Evaluation::Ready(curried(context, def, &signature, values, span)?));
    };
    if frame.runtime {
        let bound = declared.params().iter().zip(&args);
        let bindings = bound.clone().map(|(param, value)| param.comptime().then(|| value.clone()));
        let callee = curried(context, def, &signature, bindings.collect(), span)?;
        let runtime = bound.filter(|(param, _)| !param.comptime());
        let runtime = runtime.map(|(_, value)| value.clone()).collect::<Vec<_>>();
        let return_type = *signature.return_type();
        return contract::call(context, def, &args, span, |context| {
            super::finish(context, &callee, &runtime, return_type, span)
        });
    }
    // A type generator yields one type for the same arguments, however often it is called
    let generator =
        !spliced && matches!(context.env().resolve_kind(*signature.return_type()), HMIRTypeKind::Type);
    if generator && let Some(value) = context.env().resolver().generated(def, &args) {
        return Ok(Evaluation::Ready(value.clone()));
    }
    let value = frame.enter(context, span, |child| {
        execute(child, function, &signature, &args, &name, span)
    })?;
    if generator {
        let resolver = context.env_mut().resolver_mut();
        resolver.record_generated(def, args, value.clone());
    }
    match spliced {
        true => staged::splice(context, value, span),
        false => Ok(Evaluation::Ready(value)),
    }
}

// The function 'def' curried with 'values'. With an argument for each of its comptime parameters, it is the
// function generated for them, which is only generated once it is lowered as a runtime operand
fn curried(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    signature: &HMIRFnSignature<HMIRTypeID>,
    values: Vec<Option<HMIRValue>>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let residual = types::residual(signature, |index| values[index].is_some());
    let ty = types::callable(context.env_mut(), residual, span);
    let callee = symbol::value(context.env_mut(), def)?;
    Ok(value::curried(ty, callee, values))
}

// The function generated for 'callee' curried with the arguments of its comptime parameters, where 'ty' is the
// type of the curried function
pub(crate) fn template(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    comptime_bindings: &[Option<HMIRValue>],
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRFunctionID> {
    let referent = context.env().referent(ty);
    let (def, signature) = match (value::as_function(callee), context.env().resolve_kind(referent)) {
        (Some(def), HMIRTypeKind::Function(signature)) => (def, signature.clone()),
        _ => {
            let unavailable = "this function".into();
            return Err(hmir_error(span, &mir::COMPTIME_UNAVAILABLE, unavailable));
        }
    };
    let comptime_bindings = comptime_bindings.iter().flatten().cloned().collect();
    let generation = Generation::Template { def, comptime_bindings };
    lower::declare(context.env_mut(), generation, &signature, span)
}

// The signature that 'function' has for the arguments of its comptime parameters
pub(crate) fn signature(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    function: &HMIRFunction,
    comptime_bindings: &[HMIRValue],
    span: &TokenRange,
) -> CXResult<HMIRFnSignature<HMIRTypeID>> {
    let comptime = FnContext::Comptime(ComptimeContext::new());
    let mut context = LocalLoweringContext::new(env, None, comptime).sourced(def.brand());
    let mut bindings = comptime_bindings.iter().cloned();
    let params = function.signature().params().iter();
    let values = params.map(|param| param.comptime().then(|| bindings.next()).flatten());
    let mut frame = Frame::new(function, def, ComptimeContext::new(), true);
    frame.instantiate(&mut context, values.collect(), span)?;
    frame.signature(&mut context, span)
}

fn name(context: &LocalLoweringContext, def: HMIRDefID) -> String {
    let source = context.env().symbols().resolve_definition(def);
    source.name().to_string()
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
            return Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ()));
        }
        Err(EvalExit::Stop(StopReason::Runtime(stop))) => {
            unreachable!("runtime stop {stop:?} in a comptime function")
        }
        Err(EvalExit::Diagnostic(error)) => return Err(error),
        Ok(Evaluation::Incomplete(incomplete)) => return Err(incomplete.error()),
        Ok(Evaluation::Ready(_)) => {
            unreachable!("a function body returns or diagnoses fallthrough")
        }
    };
    let value = comptime::converge(context, value, span)?;
    let env = context.env();
    if let Some(slot) = context.comptime().unconsumed(|ty| env.is_nodrop(ty)) {
        let place = slot
            .name()
            .map_or_else(|| "temporary".into(), |place| place.to_string());
        let discarded = place == "_";
        let args = (
            name.to_string(),
            "variable".into(),
            place,
            "function".into(),
            discarded,
        );
        return Err(hmir_error(span, &analysis::VALUE_NOT_CONSUMED, args));
    }
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
        let ty = context
            .return_type()
            .expect("function return type is evaluated");
        match context.env().resolve_kind(ty) {
            HMIRTypeKind::Void => {
                return control::return_value(context, body, None, false, root.span());
            }
            HMIRTypeKind::Unreachable => {
                return Err(hmir_error(root.span(), &mir::COMPTIME_UNREACHABLE, ()).into());
            }
            HMIRTypeKind::Int(_) if name.as_str() == "main" => {}
            _ => {
                return Err(hmir_error(root.span(), &mir::FUNCTION_RETURN, name.to_string()).into());
            }
        }
    }
    control::fallthrough(context, body, name.as_str() == "main", root.span())
}
