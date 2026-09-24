mod aggregate;
mod float;
mod internal;
mod pointer;

use cx_log::{CXResult, catalogue::mir};
use cx_mir::{MIRComptimeBody, MIRIntrinsic};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext,
    execution::{
        arithmetic::execute_integer_op,
        engine::{Engine, ExecutionFrame},
    },
    log::comptime_error,
};

pub(crate) fn execute<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &MIRComptimeBody<'_>,
    intrinsic: &MIRIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    match intrinsic {
        MIRIntrinsic::Int(op) => execute_integer_op(engine, frame, body, op, range),
        MIRIntrinsic::Float(op) => float::execute(engine, frame, body, op, range),
        MIRIntrinsic::Pointer(op) => pointer::execute(engine, frame, body, op, range),
        MIRIntrinsic::Aggregate(op) => aggregate::execute(engine, frame, body, op, range),
        MIRIntrinsic::Internal(op) => internal::execute(engine, frame, body, op, range),
        MIRIntrinsic::VA(op) => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                format!("variadic operation {op:?}"),
            ),
        ),
    }
}
