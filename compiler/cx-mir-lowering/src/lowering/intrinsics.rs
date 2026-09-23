use cx_lmir::LMIRInstructionKind;
use cx_mir::{MIRIntrinsic, MIRTarget};

use crate::context::FunctionContext;

use super::values::{target_type, write_target};

mod aggregate;
mod arithmetic;
mod internal;

pub(super) fn lower_intrinsic(context: &mut FunctionContext<'_, '_>, op: &MIRIntrinsic) {
    match op {
        MIRIntrinsic::Int(op) => arithmetic::integer(context, op),
        MIRIntrinsic::Float(op) => arithmetic::float(context, op),
        MIRIntrinsic::Pointer(op) => arithmetic::pointer(context, op),
        MIRIntrinsic::Aggregate(op) => aggregate::lower(context, op),
        MIRIntrinsic::Internal(op) => internal::lower(context, op),
        MIRIntrinsic::VA(op) => internal::variadic(context, op),
    }
}

pub(super) fn output(
    context: &mut FunctionContext<'_, '_>,
    target: MIRTarget,
    kind: LMIRInstructionKind,
) {
    let ty = context.ty(target_type(context, target));
    let value = context.temp(kind, ty);
    write_target(context, target, value);
}
