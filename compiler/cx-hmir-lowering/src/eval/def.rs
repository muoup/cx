use cx_hmir::{
    HMIRDefID, HMIRDefKind, HMIRDefRef, HMIRFunction,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics},
};
use cx_log::catalogue::{mir, typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{ComptimeContext, FnContext, LocalLoweringContext},
    eval::{
        EvalResult, complete, eval_comptime, eval_type,
        flow::{Evaluation, ready},
        types,
    },
    log::hmir_error,
    lower::{global, is_staged},
    value::{HMIRPlace, HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub(crate) fn eval_def(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    def: &HMIRDefRef,
) -> EvalResult<HMIRValue> {
    let HMIRDefRef::Local(id) = def else {
        todo!("reference to an external definition")
    };
    let resolved = context.env_mut().resolve_def(def);

    match resolved.kind() {
        HMIRDefKind::Function(function) => function_value(context, span, *id, function),
        HMIRDefKind::ComptimeGlobal(global) => {
            let mut child = context.child(None, FnContext::Comptime(ComptimeContext::new()), None);
            let ty = ready!(eval_type(&mut child, global.body(), global.ty())?.description());
            let value = eval_comptime(&mut child, global.body(), global.initializer(), Some(ty))?;

            match value.kind() {
                HMIRValueKind::Constant(HMIRConstant::Type(ty)) => {
                    complete::type_result(context, *ty, span)
                }
                _ => Ok(Evaluation::Ready(value)),
            }
        }
        HMIRDefKind::Global(global) => {
            if matches!(context.context(), FnContext::Comptime(_)) {
                return Err(hmir_error(
                    span,
                    &mir::COMPTIME_UNAVAILABLE,
                    format!("global '{}'", resolved.name()),
                )
                .into());
            }
            let (ty, _) = global::declare(global, context.env_mut())?;
            let value = HMIRValue::place(ty, HMIRPlace::Global(*id));
            Ok(Evaluation::Ready(emit::reference(context, &value, span)?))
        }
        HMIRDefKind::Type(ty) => {
            context.env_mut().resolver_mut().mark_incomplete(*ty);
            complete::type_result(context, *ty, span)
        }
    }
}

pub(crate) fn type_value(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> cx_log::CXResult<HMIRValue> {
    if matches!(context.context(), FnContext::Runtime(_)) {
        let subject = (
            format!("type '{}'", types::name(context.env(), ty)),
            "a runtime expression".into(),
        );
        return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, subject).into());
    }

    let universe = types::universe(context.env_mut(), span);
    Ok(HMIRValue::constant(universe, HMIRConstant::Type(ty)))
}

// The reference is what declares the function to the module, as a function without a body is never generated
fn function_value(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    def: HMIRDefID,
    function: &HMIRFunction,
) -> EvalResult<HMIRValue> {
    if is_staged(function) {
        todo!("reference to a function with comptime parameters")
    }

    let mut child = context.child(None, FnContext::Comptime(ComptimeContext::new()), None);
    let signature = ready!(types::eval_signature(
        &mut child,
        function.body(),
        function.signature()
    ));

    let env = context.env_mut();
    let prototype = types::lower_prototype(env, function.prototype(), &signature, span)?;
    env.module_mut().declare_function(prototype);

    let ty = env.resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::Function(signature),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));
    let function = HMIRConstant::Function {
        def,
        args: Vec::new(),
    };

    Ok(Evaluation::Ready(HMIRValue::constant(ty, function)))
}
