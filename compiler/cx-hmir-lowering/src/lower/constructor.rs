use cx_hmir::ty::HMIRTypeID;
use cx_log::CXResult;
use cx_mir::{
    MIRFunction, MIRFunctionID, MIRInstructionKind, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::{
        HMIREnvironment,
        function::{FnContext, LocalLoweringContext, RuntimeContext},
    },
    eval::aggregate::variant,
};

pub(crate) fn lower(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    index: usize,
    function: MIRFunctionID,
) -> CXResult<MIRFunction> {
    let span = TokenRange::internal();
    let prototype = env.module().function(function).prototype().clone();
    let runtime = FnContext::Runtime(RuntimeContext::new(prototype));
    let mut context = LocalLoweringContext::new(env, None, runtime);

    let scope = emit::open_scope(&mut context);
    let payload = match variant::payload(context.env(), ty, index) {
        Some(payload) => {
            let param = runtime::storage::parameter(&mut context, payload, None, &span)?;
            let moved = emit::storage::lift(&mut context, &param, &span)?;
            emit::storage::invalidate(&mut context, &param, MIRInvalidationKind::Move, &span);
            Some(moved)
        }
        None => None,
    };
    let value = variant::construct(&mut context, ty, index, payload, &span)?;
    let value = Some(runtime::value(&mut context, &value, &span)?);
    runtime::drop_scope(&mut context, scope, &span);
    context
        .runtime_mut()
        .emit(MIRInstructionKind::Return { value }, span.clone());
    emit::close_scope(&mut context, &span);

    Ok(context.into_runtime().finish())
}
