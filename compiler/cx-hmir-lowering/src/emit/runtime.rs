pub mod op;

use cx_hmir::{
    HMIRBinaryOp,
    ty::{context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::CXResult;
use cx_mir::{MIRInstructionKind, MIRInternalIntrinsic, MIRIntrinsic, MIRPlaceID, expr::instruction::MIRInvalidationKind};
use cx_tokens::TokenRange;

use crate::{
    env::function::{LocalLoweringContext, runtime::RuntimeScope},
    eval::types,
    value::{HMIRValue, immediate::HMIRImmediate},
};

pub fn open_scope(context: &mut LocalLoweringContext, span: &TokenRange) -> usize {
    let runtime = context.runtime_mut();
    let scopes = runtime.scopes_mut();
    let parent = scopes.top();

    scopes.push(RuntimeScope::new(false), Some(parent), Bindings::default())
}

pub fn close_scope(context: &mut LocalLoweringContext, span: &TokenRange) {
    let index = context.runtime().scopes().top();
    drop_scope(context, index, span);
    context.runtime_mut().scopes_mut().pop();
}

pub fn drop_scope(context: &mut LocalLoweringContext, index: usize, span: &TokenRange) {
    let Some(scope) = context.runtime().scopes().control(index).mir() else {
        return;
    };

    let places = context
        .runtime()
        .builder()
        .places()
        .iter()
        .enumerate()
        .filter(|(_, place)| place.scope() == scope)
        .map(|(index, _)| MIRPlaceID::new(index))
        .collect::<Vec<_>>();

    for place in places.into_iter().rev() {
        let invalidate = MIRInstructionKind::Invalidate {
            place,
            kind: MIRInvalidationKind::Drop,
        };
        context.runtime_mut().emit(invalidate, span.clone());
    }
}

pub fn null_check(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    assert!(context.env().is_pointer(value.ty()));

    let bool = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let compare_to = HMIRValue::new(value.ty(), HMIRImmediate::Int(0));

    let comparison = op::binary(context, HMIRBinaryOp::Neq, &value, &compare_to, bool, span)?;

    context.runtime_mut()
        .emit(
            MIRInstructionKind::IntrinsicOp(
                MIRIntrinsic::Internal(
                    MIRInternalIntrinsic::Assert { 
                        condition: comparison, 
                        message: Some("Attempted to dereference a null pointer".into())
                    }
                )
            ),
            span.clone()
        );

    Ok(())
}
