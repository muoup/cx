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
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

use super::{
    EvalResult, coerce, comptime, contract, control, eval, eval_type,
    flow::{self, ComptimeStop, Evaluation, StopReason, ready},
    storage, types,
};

struct Frame<'a> {
    function: &'a HMIRFunction,
    def: HMIRDefID,
    state: ComptimeContext,
    return_type: Option<HMIRTypeID>,
}

impl Frame<'_> {
    fn parameter(
        &mut self,
        caller: &mut LocalLoweringContext,
        index: usize,
        span: &TokenRange,
    ) -> CXResult<HMIRTypeID> {
        let function = self.function;
        self.enter(caller, span, |child| {
            let expr = *function.signature().params()[index].ty();
            let span = function.body().expr(expr).span();
            let ty = flow::top(eval_type(child, function.body(), expr)?.description())?;
            child.force_type(ty, span)?;
            let ty = types::parameter(child, ty, span);
            if matches!(
                child.env().resolve_kind(ty),
                HMIRTypeKind::ReferenceTo(_) | HMIRTypeKind::StagedExpr { .. }
            ) {
                return Err(hmir_error(
                    span,
                    &mir::COMPTIME_INVALID_OPERATION,
                    "a reference or staged-expression parameter".into(),
                ));
            }
            Ok(ty)
        })
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
        if matches!(
            caller.env().resolve_kind(return_type),
            HMIRTypeKind::StagedExpr { .. }
        ) {
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
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let HMIRValueKind::Constant(HMIRConstant::Function { def, args: applied }) = callee.kind()
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
    if !applied.is_empty()
        || declared.is_variadic()
        || (declared.stage() == HMIRFunctionStage::Runtime
            && symbol::needs_specialization(function))
    {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_INVALID_OPERATION,
            "a variadic, partially applied or runtime-specialized function call".into(),
        ));
    }
    if args.len() != declared.params().len() {
        return Err(hmir_error(
            span,
            &typecheck::ARGUMENT_COUNT,
            (
                format!("'{}'", source.name()),
                declared.params().len(),
                args.len(),
                false,
            ),
        ));
    }
    let Some(_) = function.def() else {
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
    };
    let mut params = Vec::with_capacity(args.len());
    let mut values = Vec::with_capacity(args.len());
    for (index, (param, arg)) in declared.params().iter().zip(args).enumerate() {
        let ty = frame.parameter(context, index, span)?;
        let arg_span = caller_body.expr(*arg).span();
        let evaluated = eval(context, caller_body, *arg)?;
        let value = ready!(evaluated.complete(context, caller_body, Some(ty)));
        let value = coerce::implicit(context, value, ty, arg_span)?;
        let value = flow::top(comptime::export(context, value, arg_span))?;
        if let HMIRValueKind::Constant(HMIRConstant::Type(ty)) = value.kind() {
            context.prepare_type(*ty, arg_span)?;
        }
        frame.bind(context, index, ty, &value, span)?;
        params.push(HMIRFnParam::new(param.comptime(), ty));
        values.push(value);
    }
    let signature = frame.signature(context, params, span)?;
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
        };
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
            other.bind(context, index, ty, value, span)?;
            params.push(HMIRFnParam::new(param.comptime(), ty));
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
    Ok(Evaluation::Ready(value))
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
