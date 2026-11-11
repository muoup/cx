use cx_hmir::{
    HMIRDefID, HMIRFnSignature,
    ty::{HMIRTypeID, HMIRTypeKind},
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use super::contract;
use crate::{
    emit,
    env::{function::LocalLoweringContext, generation::Generated},
    eval::{EvalResult, flow::Evaluation, types},
    lower, symbol,
    value::{self, HMIRValue},
};

// Calls the function generated from 'def' for the values of its comptime parameters
pub fn apply(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    signature: &HMIRFnSignature<HMIRTypeID>,
    values: Vec<HMIRValue>,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let return_type = *signature.return_type();
    if context.is_unevaluated() {
        let placeholder = emit::runtime::placeholder(context, return_type, span)?;
        return Ok(Evaluation::Ready(placeholder));
    }
    let params = signature.params().iter().zip(&values);
    let bindings = params
        .clone()
        .map(|(param, value)| param.comptime().then(|| value.clone()))
        .collect();
    let args = params
        .filter(|(param, _)| !param.comptime())
        .map(|(_, value)| value.clone())
        .collect::<Vec<_>>();
    let callee = declare(context, def, signature, bindings, span)?;
    contract::call(context, def, &values, span, |context| {
        super::finish(context, &callee, &args, return_type, span)
    })
}

// The function generated from 'def' for 'bindings', as 'def' curried with them
pub fn declare(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    signature: &HMIRFnSignature<HMIRTypeID>,
    bindings: Vec<Option<HMIRValue>>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if !context.is_unevaluated() {
        let source = Generated::Template {
            def,
            bindings: bindings.clone(),
        };
        lower::declare(context.env_mut(), source, signature, span)?;
    }
    let residual = types::residual(signature, |index| bindings[index].is_some());
    let ty = types::realize(context.env_mut(), HMIRTypeKind::Function(residual), span);
    let callee = symbol::binding(context.env_mut(), def, None)?;
    Ok(value::curried(ty, callee, bindings))
}
