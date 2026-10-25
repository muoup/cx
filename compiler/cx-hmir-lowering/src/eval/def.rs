use cx_hmir::{
    HMIRDefID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use crate::{
    env::function::{FnContext, LocalLoweringContext},
    eval::{EvalResult, complete, flow::Evaluation, types},
    log::hmir_error,
    lower::global,
    symbol,
    value::{
        HMIRPlace, HMIRValue, HMIRValueKind,
        constant::{HMIRConstant, HMIRFunctionRef},
    },
};

pub(crate) fn eval_def(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    id: HMIRDefID,
) -> EvalResult<HMIRValue> {
    let point = context.visible_through();
    let value = symbol::binding(context.env_mut(), id, point)?;
    match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Type(ty)) => {
            complete::type_result(context, *ty, span)
        }
        HMIRValueKind::Constant(HMIRConstant::Function {
            function: HMIRFunctionRef::Def(def),
            ..
        }) => function_value(context, span, *def, value),
        HMIRValueKind::Constant(HMIRConstant::Address(address))
            if let Some(HMIRPlace::Global(def)) = address.whole_place() =>
        {
            let linkage = symbol::linkage(context.env_mut(), def, point)?;
            if def.brand() == context.env().symbols().local_unit().id()
                || linkage == LinkageMode::Static
            {
                context.env_mut().generation_mut().request(def);
            }
            if matches!(context.context(), FnContext::Runtime(_)) && !context.is_unevaluated() {
                let ty = symbol::value_type(context.env_mut(), def, None)?;
                global::declare(context.env_mut(), def, ty, span)?;
            }
            Ok(Evaluation::Ready(value))
        }
        _ => Ok(Evaluation::Ready(value)),
    }
}

pub(crate) fn type_value(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if matches!(context.context(), FnContext::Runtime(_)) && !context.is_unevaluated() {
        let subject = (
            format!("type '{}'", types::name(context.env(), ty)),
            "a runtime expression".into(),
        );
        return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, subject).into());
    }

    let universe = types::universe(context.env_mut(), span);
    Ok(HMIRValue::constant(universe, HMIRConstant::Type(ty)))
}

fn function_value(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    def: HMIRDefID,
    value: HMIRValue,
) -> EvalResult<HMIRValue> {
    let point = context.visible_through();
    if matches!(context.context(), FnContext::Runtime(_)) && !context.is_unevaluated() {
        if matches!(
            context.env().resolve_kind(value.ty()),
            HMIRTypeKind::FunctionItem(_)
        ) {
            return Ok(Evaluation::Ready(value));
        }
        let prototype = types::lower_prototype(context.env_mut(), def, point, span)?;
        context.env_mut().module_mut().declare_function(prototype);
        let linkage = symbol::linkage(context.env_mut(), def, point)?;
        if def.brand() == context.env().symbols().local_unit().id()
            || linkage == LinkageMode::Static
        {
            context.env_mut().generation_mut().request(def);
        }
    }

    Ok(Evaluation::Ready(value))
}
