use cx_hmir::{
    HMIRDefID, HMIRFnSignature,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics},
};
use cx_mir::MIRFnPrototype;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    emit,
    env::{function::LocalLoweringContext, generation::Specialization},
    eval::{EvalResult, call, contract, flow::Evaluation, safety, types},
    symbol,
    value::{
        HMIRValue, HMIRValueKind,
        constant::{HMIRConstant, HMIRFunctionRef},
    },
};

pub fn apply(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    signature: &HMIRFnSignature<HMIRTypeID>,
    values: Vec<HMIRValue>,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    if context.is_unevaluated() {
        let ty = *signature.return_type();
        return Ok(Evaluation::Ready(emit::scratch(context, ty, span)?));
    }
    safety::require_call(context, span, signature.contract().is_safe(), true)?;
    let mut bindings = Vec::with_capacity(values.len());
    let mut args = Vec::new();
    let mut runtime = Vec::new();
    let mut params = Vec::new();
    for (param, value) in signature.params().iter().zip(&values) {
        if param.comptime() {
            let HMIRValueKind::Constant(constant) = value.kind() else {
                unreachable!("comptime arguments were exported before specialization")
            };
            args.push(constant.clone());
            bindings.push(Some(value.clone()));
        } else {
            bindings.push(None);
            runtime.push(value.clone());
            params.push(param.clone());
        }
    }
    let concrete = HMIRFnSignature::new(
        signature.stage(),
        params,
        *signature.return_type(),
        signature.is_variadic(),
        signature.contract().clone(),
    );
    if context
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
    let function = HMIRFunctionRef::Def(def);
    let callee = HMIRValue::constant(ty, HMIRConstant::Function { function, args });
    contract::call(context, def, &values, span, |context| {
        call::finish(context, &callee, &runtime, *signature.return_type(), span)
    })
}
