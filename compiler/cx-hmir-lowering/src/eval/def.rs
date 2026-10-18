use cx_hmir::{
    HMIRDefID, HMIRDefKind, HMIRDefRef, HMIRFunction,
    expr::constant::HMIRConstant,
    ty::{HMIRType, HMIRTypeKind, aggregate::HMIRTypeSemantics},
    value::HMIRValue,
};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{ComptimeContext, FnContext, LocalLoweringContext},
    eval::{EvalResult, coerce::implicit, eval, eval_type, types},
    log::hmir_error,
    lower::is_staged,
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
            let mut child = context.child(None, FnContext::Comptime(ComptimeContext::new()));
            let ty = eval_type(&mut child, global.body(), global.ty())?;
            let value = eval(&mut child, global.body(), global.initializer())?;
            let value = implicit(&mut child, value, ty, span)?;

            Ok(emit::read(&mut child, value, span)?)
        }
        HMIRDefKind::Global(_) => todo!("global places: globals are not lowered yet"),
        HMIRDefKind::Type(_) => Err(hmir_error(
            span,
            &typecheck::UNEXPECTED_KIND,
            (format!("type '{}'", resolved.name()), "a value".into()),
        )
        .into()),
    }
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

    let mut child = context.child(None, FnContext::Comptime(ComptimeContext::new()));
    let signature = types::eval_signature(&mut child, function.body(), function.signature())?;

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

    Ok(HMIRValue::constant(ty, function))
}
