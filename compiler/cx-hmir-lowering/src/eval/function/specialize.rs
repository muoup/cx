use cx_hmir::{
    HMIRDefID, HMIRFnSignature,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics},
};
use cx_log::CXResult;
use cx_mir::MIRFnPrototype;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    emit,
    env::{function::LocalLoweringContext, generation::Specialization},
    eval::{EvalResult, call, contract, flow::Evaluation, types},
    symbol,
    value::{HMIRValue, constant::HMIRFunctionRef},
};

pub fn apply(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    signature: &HMIRFnSignature<HMIRTypeID>,
    values: Vec<HMIRValue>,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    if context.is_unevaluated() {
        return Ok(Evaluation::Ready(emit::scratch(
            context,
            *signature.return_type(),
            span,
        )?));
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
        call::finish(context, &callee, &runtime, *signature.return_type(), span)
    })
}

pub fn declare(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    signature: &HMIRFnSignature<HMIRTypeID>,
    bindings: Vec<Option<HMIRValue>>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let args = bindings.iter().flatten().cloned().collect::<Vec<_>>();
    let params = signature
        .params()
        .iter()
        .filter(|param| !param.comptime())
        .cloned()
        .collect();
    let concrete = HMIRFnSignature::new(
        signature.stage(),
        params,
        *signature.return_type(),
        signature.is_variadic(),
        signature.contract().clone(),
    );
    if !context.is_unevaluated()
        && context
            .env()
            .generation()
            .generated(HMIRFunctionRef::Def(def), &args)
            .is_none()
    {
        let env = context.env_mut();
        let original =
            symbol::link_name(env, def, None)?.expect("a specialization names a function");
        let name = CXIdent::new(format!(
            "{}.__cx_specialize.{}",
            original,
            env.generation().generated_count()
        ));
        let prototype = MIRFnPrototype::new(
            types::lower_signature(env, &concrete, span)?,
            LinkageMode::Static,
            name,
            Some(original),
        );
        let function = env.module_mut().declare_function(prototype);
        env.generation_mut().specialize(
            args.clone(),
            Specialization::new(def, function, signature.clone(), bindings),
        );
    }
    let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::Function(concrete),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));
    let ty = types::pointer(context.env_mut(), ty, span);
    let callee = symbol::binding(context.env_mut(), def, None)?;
    Ok(HMIRValue::curried(ty, callee, args))
}
