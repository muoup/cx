use cx_hmir::{
    HMIRBody, HMIRDefID,
    expr::HMIRExprID,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::{MIRBlockTarget, MIRInstructionKind};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit::runtime,
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        EvalResult, coerce, control, eval,
        flow::{Evaluation, RuntimeStop, ready},
        types,
    },
    log::hmir_error,
    value::{HMIRValue, constant::HMIRConstant},
};

pub(crate) fn eval_label(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    name: &CXIdent,
    statement: HMIRExprID,
) -> EvalResult<HMIRValue> {
    if matches!(context.context(), FnContext::Runtime(_)) && !control::unevaluated(context) {
        let target = context.runtime_mut().label(name);
        runtime::jump(context, target, span.clone());
        runtime::switch_to(context, target);
    }
    eval(context, body, statement)
}

pub(crate) fn eval_goto(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    name: &CXIdent,
) -> EvalResult<HMIRValue> {
    jumpable(context, span)?;
    if !control::unevaluated(context) {
        let target = context.runtime_mut().label(name);
        runtime::jump(context, target, span.clone());
    }
    Ok(Evaluation::Stop(RuntimeStop::Diverged.into()))
}

pub(crate) fn eval_indirect_goto(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    target: HMIRExprID,
) -> EvalResult<HMIRValue> {
    jumpable(context, span)?;
    let value = ready!(eval(context, body, target));
    let ty = address_type(context, true, span);
    let value = coerce::implicit(context, value, ty, span)?;
    let address = runtime::lower_value(context, &value, span)?;
    let targets = context
        .runtime()
        .builder()
        .address_labels()
        .iter()
        .map(|(_, block)| MIRBlockTarget::new(*block))
        .collect();
    let jump = MIRInstructionKind::IndirectJump { address, targets };
    context.runtime_mut().emit(jump, span.clone());
    Ok(Evaluation::Stop(RuntimeStop::Diverged.into()))
}

pub(crate) fn eval_label_address(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    function: Option<HMIRDefID>,
    name: &CXIdent,
) -> EvalResult<HMIRValue> {
    let prototype = match (function, context.context()) {
        (Some(def), _) => types::lower_prototype(context.env_mut(), def, None, span)?,
        (None, FnContext::Runtime(runtime)) => runtime.builder().prototype().clone(),
        (None, FnContext::Comptime(_)) => {
            let required = ("label address".into(), "a function".into());
            return Err(hmir_error(span, &typecheck::REQUIRED_CONTEXT, required));
        }
    };
    let function = context.env_mut().module_mut().declare_function(prototype);
    let ty = address_type(context, false, span);
    let label = name.to_string();
    let address = HMIRConstant::BlockAddress { function, label };
    Ok(Evaluation::Ready(HMIRValue::constant(ty, address)))
}

fn jumpable(context: &LocalLoweringContext, span: &TokenRange) -> CXResult<()> {
    match context.context() {
        FnContext::Comptime(_) => {
            let operation = "'goto'".into();
            Err(hmir_error(
                span,
                &mir::COMPTIME_INVALID_OPERATION,
                operation,
            ))
        }
        FnContext::Runtime(runtime) if runtime.deferred() => {
            Err(hmir_error(span, &typecheck::DEFER_JUMP, "jump".into()))
        }
        FnContext::Runtime(_) => Ok(()),
    }
}

fn address_type(
    context: &mut LocalLoweringContext,
    constant: bool,
    span: &TokenRange,
) -> HMIRTypeID {
    let semantics = HMIRTypeSemantics {
        constant,
        ..Default::default()
    };
    let void = HMIRType::new(HMIRTypeKind::Void, semantics, span.clone(), None);
    let void = context.env_mut().resolver_mut().realize_type(void);
    types::pointer(context.env_mut(), void, span)
}
