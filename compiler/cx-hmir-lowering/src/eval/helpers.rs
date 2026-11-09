use cx_hmir::ty::context::HMIRTypeContext;
use cx_intrinsics::{PointerCompareOp, PointerIntrinsic};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{emit::runtime, env::function::LocalLoweringContext, value::{HMIRValIntrinsic, HMIRValue, immediate::HMIRImmediate}};

pub fn null_check(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    assert!(context.env().is_pointer(value.ty()));

    let compare_to = HMIRValue::new(value.ty(), HMIRImmediate::Int(0));
    let comparison_op = PointerIntrinsic::Compare {
        op: PointerCompareOp::Neq,
        lhs: value,
        rhs: compare_to,
    };

    let comparison =
        emit::intrinsic(context, &HMIRValIntrinsic::Pointer(comparison_op), span)?;

    emit::intrinsic(
        context,
        &HMIRValIntrinsic::Internal(InternalIntrinsic::Assert {
            condition: comparison,
            message: Some("Attempted to dereference a null pointer".into()),
        }),
        span,
    )?;

    Ok(())
}