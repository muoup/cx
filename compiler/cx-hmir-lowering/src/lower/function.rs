use cx_hmir::{HMIRDefID, HMIRFnSignature, HMIRFunction, ty::HMIRTypeID};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::{
        HMIREnvironment,
        function::{FnContext, LocalLoweringContext, RuntimeContext},
        generation::Specialization,
    },
    eval::{
        EvalResult,
        call::{self, contract},
        flow, types,
    },
    lower::{mir, types::lower_prototype},
    value::HMIRValue,
};

pub(crate) fn lower<'hmir>(
    env: &mut HMIREnvironment<'_, 'hmir>,
    id: HMIRDefID,
    function: &HMIRFunction,
    signature: &HMIRFnSignature<HMIRTypeID>,
    specialization: Option<&Specialization>,
) -> CXResult<()> {
    let body = function.body();
    let def = function
        .def()
        .expect("Runtime function lowered without a definition");
    let span = body.expr(def.root()).span();

    let prototype = match specialization {
        Some(specialization) => env
            .module()
            .function(specialization.function())
            .prototype()
            .clone(),
        None => lower_prototype(env, id, None, span)?,
    };
    let runtime = FnContext::Runtime(RuntimeContext::new(prototype));
    let mut context = LocalLoweringContext::new(env, Some(function.signature()), runtime)
        .sourced(id.brand())
        .at(body.visible_through())
        .with_return_type(*signature.return_type());

    for name in def.address_labels() {
        let runtime = context.runtime_mut();
        let block = runtime.label(name);
        runtime
            .builder_mut()
            .add_address_label(name.to_string(), block);
    }
    let root = emit::open_scope(&mut context, span);
    let scopes = context.runtime_mut().scopes_mut();
    scopes.control_mut(root).set_return(id.brand(), body.visible_through());
    let completion = definition(&mut context, function, signature, specialization, span);
    emit::close_scope(&mut context, span);
    flow::top(flow::catch_diverged(completion))?;

    let lowered = context.into_runtime().finish();
    let module = env.module_mut();
    let id = match specialization {
        Some(specialization) => specialization.function(),
        None => module.declare_function(lowered.prototype().clone()),
    };
    module.define_function(id, lowered);
    Ok(())
}

fn definition(
    context: &mut LocalLoweringContext,
    function: &HMIRFunction,
    signature: &HMIRFnSignature<HMIRTypeID>,
    specialization: Option<&Specialization>,
    span: &TokenRange,
) -> EvalResult<flow::Evaluation<HMIRValue>> {
    let body = function.body();
    for (index, (param, local)) in signature.params().iter().zip(function.params()).enumerate() {
        let bound = specialization.and_then(|specialization| specialization.bindings()[index].as_ref());
        let value = match bound {
            Some(value) => value.clone(),
            None => {
                let decl = body.local(*local);
                let reference = types::reference(context.env_mut(), *param.ty(), None, decl.span());
                mir::parameter(context, *param.ty(), decl.name().cloned(), decl.span())?
                    .with_ty(reference)
            }
        };
        emit::bind_local(context, *local, value);
    }
    contract::enter(context, function.params(), span)?;

    call::body(context, function)
}
