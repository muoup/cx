use chl_hmir::ty::HMIRTypeID;
use chl_log::CHLResult;
use chl_mir::{MIRBasicBlockID, MIRBlockTarget, MIRInstructionKind, MIRValue};
use chl_tokens::TokenRange;
use chl_util::identifier::CHLIdent;

use crate::{
    emit::runtime,
    env::function::{LocalLoweringContext, runtime::Emission},
    value::HMIRValue,
};

pub fn block(context: &mut LocalLoweringContext, name: &str) -> MIRBasicBlockID {
    context
        .runtime_mut()
        .builder_mut()
        .add_block(Some(CHLIdent::new(name)))
}

pub fn current_block(context: &LocalLoweringContext) -> MIRBasicBlockID {
    context.runtime().builder().current_block()
}

pub fn switch_to(context: &mut LocalLoweringContext, block: MIRBasicBlockID) {
    let runtime = context.runtime_mut();

    runtime.builder_mut().switch_to(block);
    if runtime.emission() != Emission::Unevaluated {
        runtime.set_emission(Emission::Enabled);
    }
}

pub fn jump(context: &mut LocalLoweringContext, target: MIRBasicBlockID, span: &TokenRange) {
    let target = MIRBlockTarget::new(target);
    context
        .runtime_mut()
        .emit(MIRInstructionKind::Jump { target }, span.clone())
}

pub fn branch(
    context: &mut LocalLoweringContext,
    cond: MIRValue,
    true_target: MIRBasicBlockID,
    false_target: MIRBasicBlockID,
    span: &TokenRange,
) {
    let branch = MIRInstructionKind::Branch {
        cond,
        true_target: MIRBlockTarget::new(true_target),
        false_target: MIRBlockTarget::new(false_target),
    };

    context.runtime_mut().emit(branch, span.clone())
}

pub fn call(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    args: &[HMIRValue],
    result: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<HMIRValue> {
    let callee = runtime::value(context, callee, span)?;
    let args = args
        .iter()
        .map(|arg| runtime::value(context, arg, span))
        .collect::<CHLResult<_>>()?;

    let out = runtime::register(context, result, span)?;
    let mir_out = context.runtime().register(out);

    context.runtime_mut().emit(
        MIRInstructionKind::Call {
            out: Some(mir_out),
            callee,
            args,
        },
        span.clone(),
    );

    Ok(HMIRValue::new(result, out))
}
