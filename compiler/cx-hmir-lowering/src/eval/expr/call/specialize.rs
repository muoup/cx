use cx_hmir::{
    HMIRDefID, HMIRFnSignature,
    ty::{HMIRTypeID, HMIRTypeKind},
};
use cx_log::CXResult;
use cx_mir::MIRFnPrototype;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use super::contract;
use crate::{
    emit,
    env::{function::LocalLoweringContext, generation::Specialization},
    eval::{EvalResult, flow::Evaluation, types},
    lower::types::lower_signature,
    symbol,
    value::{HMIRValue, constant::HMIRFunctionRef},
};

pub(super) fn apply(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    signature: &HMIRFnSignature<HMIRTypeID>,
    values: Vec<HMIRValue>,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    if context.is_unevaluated() {
        return Ok(Evaluation::Ready(emit::scratch(context, *signature.return_type(), span)?));
    }
    let bindings = signature
        .params()
        .iter()
        .zip(&values)
        .map(|(param, value)| param.comptime().then(|| value.clone()))
        .collect();
    let callee = declare(context, def, signature, bindings, span)?;
    let runtime = signature
        .params()
        .iter()
        .zip(&values)
        .filter(|(param, _)| !param.comptime())
        .map(|(_, value)| value.clone())
        .collect::<Vec<_>>();
    contract::call(context, def, &values, span, |context| {
        super::finish(context, &callee, &runtime, *signature.return_type(), span)
    })
}

pub(super) fn declare(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    signature: &HMIRFnSignature<HMIRTypeID>,
    bindings: Vec<Option<HMIRValue>>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let args = bindings.iter().flatten().cloned().collect::<Vec<_>>();
    let params = signature.params().iter().filter(|param| !param.comptime()).cloned().collect();
    let concrete = HMIRFnSignature::new(
        signature.stage(),
        params,
        *signature.return_type(),
        signature.is_variadic(),
        signature.contract().clone(),
    );
    if !context.is_unevaluated() && context.env().generation().generated(HMIRFunctionRef::Def(def), &args).is_none() {
        let env = context.env_mut();
        let original = symbol::link_name(env, def, None)?.expect("a specialization names a function");
        let name = CXIdent::new(format!("{original}.__cx_specialize.{}", env.generation().generated_count()));
        let prototype = MIRFnPrototype::new(lower_signature(env, &concrete, span)?, LinkageMode::Static, name, Some(original));
        let function = env.module_mut().declare_function(prototype);
        env.generation_mut().specialize(
            args.clone(),
            Specialization::new(def, function, signature.clone(), bindings),
        );
    }
    let ty = types::realize(context.env_mut(), HMIRTypeKind::Function(concrete), span);
    let ty = types::pointer(context.env_mut(), ty, span);
    let callee = symbol::binding(context.env_mut(), def, None)?;
    Ok(HMIRValue::curried(ty, callee, args))
}
