use std::mem;

use cx_hmir::{
    HMIRDefID, HMIRDefKind, HMIRFnSignature, HMIRFunction, HMIRFunctionStage,
    expr::HMIRExprKind,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::function::HMIRFnParam,
};
use cx_log::{
    CXResult,
    catalogue::{analysis, mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{ComptimeContext, FnContext, LocalLoweringContext},
    eval::{
        EvalResult, coerce, comptime, control, eval, eval_type,
        flow::{self, ComptimeStop, EvalExit, Evaluation, StopReason},
        staged, types,
    },
    log::hmir_error,
    symbol,
    value::{self, HMIRValue},
};

use super::{contract, specialize};

// The comptime state of one declaration of a function as it is called. The arguments of its comptime parameters
// are bound in it as they arrive, so that the types of the parameters after them can be evaluated
pub(crate) struct Frame<'a> {
    function: &'a HMIRFunction,
    def: HMIRDefID,
    state: ComptimeContext,
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
        let (function, runtime) = (self.function, self.runtime);
        self.enter(caller, span, |child| {
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
        })
    }

    // 'value' as the argument of the parameter 'index', whose type is 'ty'
    pub(crate) fn argument(
        &mut self,
        caller: &mut LocalLoweringContext,
        index: usize,
        ty: HMIRTypeID,
        value: HMIRValue,
        span: &TokenRange,
    ) -> CXResult<HMIRValue> {
        if let Some(ty) = value::as_type(&value) {
            caller.prepare_type(ty, span)?;
        }
        let value = coerce::implicit::standard(caller, value, ty, span)?;
        let frozen = self.function.signature().params()[index].comptime();
        if self.runtime && !frozen {
            return Ok(value);
        }
        let value = match caller.is_runtime() {
            true => value,
            false => flow::top(comptime::export(caller, value, span))?,
        };
        let value = match frozen {
            true => caller.env_mut().canonical_value(value),
            false => value,
        };
        self.bind(caller, index, ty, &value, span)?;
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
            let target = emit::storage::allocate(child, ty, declaration.name().cloned(), span)?;
            emit::storage::initialize(child, &target, Some(value), span)?;
            let reference = types::reference(child.env_mut(), ty, None, span);
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
        let function = self.function;
        let declared = function.signature();
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

    // The signature that the declaration has for 'values', each of which is taken as the argument of its
    // parameter. A parameter without a value must not be one that the types after it depend on
    pub(crate) fn instantiate(
        &mut self,
        caller: &mut LocalLoweringContext,
        values: Vec<Option<HMIRValue>>,
        span: &TokenRange,
    ) -> CXResult<(HMIRFnSignature<HMIRTypeID>, Vec<Option<HMIRValue>>)> {
        let declared = self.function.signature().params();
        let mut params = Vec::with_capacity(values.len());
        let mut arguments = Vec::with_capacity(values.len());
        for (index, value) in values.into_iter().enumerate() {
            let ty = self.parameter(caller, index, span)?;
            params.push(HMIRFnParam::new(declared[index].comptime(), ty));
            arguments.push(match value {
                Some(value) => Some(self.argument(caller, index, ty, value, span)?),
                None => None,
            });
        }
        Ok((self.signature(caller, params, span)?, arguments))
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

// Whether a call to a function of type 'ty' is applied to its arguments here, rather than emitted as it is
pub(crate) fn applies(context: &LocalLoweringContext, ty: HMIRTypeID) -> bool {
    !context.is_runtime() || types::comptime_only(context.env(), ty)
}

// Whether a call to 'function' executes it here for the code that it produces, which is emitted in its place
pub(crate) fn is_spliced(context: &LocalLoweringContext, function: &HMIRFunction) -> bool {
    context.is_runtime() && function.signature().stage() == HMIRFunctionStage::Comptime
}

// Applies the function 'def' to the values given for its parameters. Given them all, a call that is emitted
// calls the function generated for its comptime arguments, and any other call executes the function here. Short
// of them all, the result is the function generated for those given, still to be called with the rest
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
    let source = unit.resolve_def(def).expect("function exists");
    let name = source.name().to_string();
    let selected = symbol::select(source, None)?;
    let Some(HMIRDefKind::Function(function)) = source.declarations()[selected].kind() else {
        unreachable!("a function constant names a function")
    };
    let spliced = is_spliced(context, function);
    let runtime = context.is_runtime() && !spliced;
    if function.def().is_none() {
        return Err(match runtime {
            true => {
                let feature = "specializing a function without a definition".into();
                hmir_error(span, &typecheck::UNSUPPORTED_FEATURE, feature)
            }
            false => hmir_error(span, &mir::COMPTIME_NO_BODY, name),
        }
        .into());
    }

    let mut frame = Frame::new(function, def, ComptimeContext::of(name.clone()), runtime);
    let (signature, values) = frame.instantiate(context, values, span)?;
    symbol::interface::check_instance(context, def, &signature, &values, runtime, span)?;
    let Some(args) = values.iter().cloned().collect::<Option<Vec<_>>>() else {
        let function = specialize::declare(context, def, &signature, values, span)?;
        return Ok(Evaluation::Ready(function));
    };
    if runtime {
        return specialize::apply(context, def, &signature, args, span);
    }
    let value = frame.enter(context, span, |child| {
        execute(child, function, &signature, &args, &name, span)
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
    let value = flow::top(comptime::export(context, value, span))?;
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
