use cx_hmir::{
    HMIRDefKind, HMIRFnSignature, HMIRFunction,
    expr::HMIRExprKind,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::mir};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{ComptimeContext, FnContext, LocalLoweringContext},
    log::hmir_error,
    symbol,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

use super::{
    EvalResult, comptime, contract, control, eval,
    flow::{self, ComptimeStop, Evaluation, StopReason, ready},
    storage, types,
};

pub(crate) fn call(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    signature: &HMIRFnSignature<HMIRTypeID>,
    args: &[HMIRValue],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let HMIRValueKind::Constant(HMIRConstant::Function { def, args: applied }) = callee.kind()
    else {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_UNAVAILABLE,
            "this function".into(),
        ));
    };
    if !applied.is_empty()
        || signature.is_variadic()
        || signature
            .params()
            .iter()
            .any(|param| context.env().is_reference(*param.ty()))
    {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_INVALID_OPERATION,
            "a variadic, reference-parameter or specialized function call".into(),
        ));
    }
    let args = args
        .iter()
        .map(|value| flow::top(comptime::export(context, value.clone(), span)))
        .collect::<CXResult<Vec<_>>>()?;
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
    let Some(definition) = function.def() else {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_NO_BODY,
            source.name().to_string(),
        ));
    };
    let mut child = context
        .child(
            Some(function.signature()),
            FnContext::Comptime(ComptimeContext::new()),
            None,
        )
        .sourced(def.brand())
        .at(function.body().visible_through())
        .with_return_type(*signature.return_type());
    child.enter_call(span)?;
    contract::check(
        &mut child,
        signature.contract(),
        &args,
        None,
        &source.name().to_string(),
        span,
    )?;
    for ((param, local), value) in signature
        .params()
        .iter()
        .zip(definition.params())
        .zip(&args)
    {
        let declaration = function.body().local(*local);
        let ty = *param.ty();
        let target = emit::allocate(
            &mut child,
            ty,
            declaration.name().cloned(),
            declaration.span(),
        )?;
        storage::initialize(&mut child, &target, value, declaration.span())?;
        let reference = types::reference(child.env_mut(), ty, declaration.span());
        emit::storage::bind_local(&mut child, *local, target.with_ty(reference));
    }
    let value = match body(&mut child, function)? {
        Evaluation::Stop(StopReason::Comptime(ComptimeStop::Return(value))) => value,
        Evaluation::Stop(StopReason::Comptime(_)) => {
            return Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ()));
        }
        Evaluation::Incomplete(incomplete) => return Err(incomplete.error()),
        _ => unreachable!("a function body returns or diagnoses fallthrough"),
    };
    let value = flow::top(comptime::export(&mut child, value, span))?;
    contract::check(
        &mut child,
        signature.contract(),
        &args,
        Some(&value),
        &source.name().to_string(),
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
