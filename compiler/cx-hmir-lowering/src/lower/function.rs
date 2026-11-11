use cx_hmir::{HMIRDefID, HMIRFnSignature, HMIRFunction, ty::HMIRTypeID};
use cx_log::CXResult;
use cx_mir::MIRFunctionID;
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::{
        HMIREnvironment,
        function::{FnContext, LocalLoweringContext, RuntimeContext},
    },
    eval::{
        EvalResult,
        call::{self, contract},
        flow, symbol, types,
    },
    lower::types::lower_prototype,
    value::HMIRValue,
};

pub(crate) fn lower<'hmir>(
    env: &mut HMIREnvironment<'_, 'hmir>,
    id: HMIRDefID,
    function: &HMIRFunction,
    template: Option<(MIRFunctionID, &[HMIRValue])>,
) -> CXResult<()> {
    let body = function.body();
    let def = function
        .def()
        .expect("Runtime function lowered without a definition");
    let span = body.expr(def.root()).span();

    let (prototype, signature) = match template {
        Some((generated, comptime_bindings)) => (
            env.module().function(generated).prototype().clone(),
            call::dependent::signature(env, id, function, comptime_bindings, span)?,
        ),
        None => (
            lower_prototype(env, id, span)?,
            symbol::signature(env, id)?,
        ),
    };
    let comptime_bindings = template.map_or(&[][..], |(_, comptime_bindings)| comptime_bindings);
    let runtime = FnContext::Runtime(RuntimeContext::new(prototype));
    let mut context = LocalLoweringContext::new(env, Some(function.signature()), runtime)
        .sourced(id.brand())
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
    scopes.control_mut(root).set_return(id.brand());
    let completion = definition(&mut context, function, &signature, comptime_bindings, span);
    emit::close_scope(&mut context, span);
    flow::top(flow::catch_diverged(completion))?;

    let lowered = context.into_runtime().finish();
    let module = env.module_mut();
    let id = match template {
        Some((generated, _)) => generated,
        None => module.declare_function(lowered.prototype().clone()),
    };
    module.define_function(id, lowered);
    Ok(())
}

fn definition(
    context: &mut LocalLoweringContext,
    function: &HMIRFunction,
    signature: &HMIRFnSignature<HMIRTypeID>,
    comptime_bindings: &[HMIRValue],
    span: &TokenRange,
) -> EvalResult<flow::Evaluation<HMIRValue>> {
    let body = function.body();
    let mut comptime_bindings = comptime_bindings.iter();
    for (param, local) in signature.params().iter().zip(function.params()) {
        let bound = param.comptime().then(|| comptime_bindings.next()).flatten();
        let value = match bound {
            Some(value) => value.clone(),
            None => {
                let decl = body.local(*local);
                let reference = types::reference(context.env_mut(), *param.ty(), None, decl.span());
                runtime::storage::parameter(context, *param.ty(), decl.name().cloned(), decl.span())?
                    .with_ty(reference)
            }
        };
        emit::storage::bind_local(context, *local, value);
    }
    contract::enter(context, function.params(), span)?;

    call::dependent::body(context, function)
}
