use cx_hmir::{HMIRDefID, ty::HMIRTypeID};
use cx_log::catalogue::{mir, typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{FnContext, LocalLoweringContext},
    eval::{EvalResult, complete, flow::Evaluation, types},
    log::hmir_error,
    lower::global,
    symbol::{self, CompletedFunction, CompletedSymbol},
    value::{HMIRPlace, HMIRValue, constant::HMIRConstant},
};

pub(crate) fn eval_def(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    id: HMIRDefID,
) -> EvalResult<HMIRValue> {
    match symbol::complete(context.env_mut(), id)? {
        CompletedSymbol::Function(function) => function_value(context, span, id, &function),
        CompletedSymbol::StagedFunction => {
            todo!("reference to a function with comptime parameters")
        }
        CompletedSymbol::Constant(value) => Ok(Evaluation::Ready(value)),
        CompletedSymbol::Global(global) => {
            if matches!(context.context(), FnContext::Comptime(_)) {
                let name = context.env().symbols().resolve_definition(id).name();
                let subject = format!("global '{name}'");
                return Err(hmir_error(span, &mir::COMPTIME_UNAVAILABLE, subject).into());
            }
            if id.brand() == context.env().symbols().local_unit().id()
                || global.linkage == cx_util::linkage::LinkageMode::Static
            {
                context.env_mut().generation_mut().request(id);
            }
            global::declare(context.env_mut(), &global, span)?;
            let value = HMIRValue::place(global.ty, HMIRPlace::Global(id));
            Ok(Evaluation::Ready(emit::reference(context, &value, span)?))
        }
        CompletedSymbol::Type(ty) => complete::type_result(context, ty, span),
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
    function: &CompletedFunction,
) -> EvalResult<HMIRValue> {
    let env = context.env_mut();
    let prototype = types::lower_prototype(env, function, span)?;
    env.module_mut().declare_function(prototype);
    if def.brand() == env.symbols().local_unit().id()
        || function.linkage == cx_util::linkage::LinkageMode::Static
    {
        env.generation_mut().request(def);
    }

    let constant = HMIRConstant::Function {
        def,
        args: Vec::new(),
    };
    Ok(Evaluation::Ready(HMIRValue::constant(
        function.ty,
        constant,
    )))
}
