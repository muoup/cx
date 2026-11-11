use cx_hmir::ty::HMIRTypeID;
use cx_intrinsics::{
    AggregateIntrinsic, FloatIntrinsic, IntIntrinsic, InternalIntrinsic, PointerIntrinsic,
    VAIntrinsic,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit::{comptime, runtime},
    env::function::{FnContext, LocalLoweringContext},
    value::{HMIRValIntrinsic, HMIRValue},
};

// An intrinsic without a result produces a value of the unit type 'result'
pub fn intrinsic(
    context: &mut LocalLoweringContext,
    intrinsic: &HMIRValIntrinsic,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match intrinsic {
        HMIRValIntrinsic::Int(op) => int(context, op, result, span),
        HMIRValIntrinsic::Float(op) => float(context, op, result, span),
        HMIRValIntrinsic::Pointer(op) => pointer(context, op, result, span),
        HMIRValIntrinsic::Aggregate(op) => aggregate(context, op, result, span),
        HMIRValIntrinsic::Internal(op) => internal(context, op, result, span),
        HMIRValIntrinsic::VA(op) => va(context, op, result, span),
    }
}

pub fn int(
    context: &mut LocalLoweringContext,
    intrinsic: &IntIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::intrinsics::int(context, intrinsic, result, span),
        FnContext::Comptime(_) => comptime::intrinsics::int(context, intrinsic, result, span),
    }
}

pub fn float(
    context: &mut LocalLoweringContext,
    intrinsic: &FloatIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::intrinsics::float(context, intrinsic, result, span),
        FnContext::Comptime(_) => comptime::intrinsics::float(context, intrinsic, result, span),
    }
}

pub fn pointer(
    context: &mut LocalLoweringContext,
    intrinsic: &PointerIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::intrinsics::pointer(context, intrinsic, result, span),
        FnContext::Comptime(_) => comptime::intrinsics::pointer(context, intrinsic, result, span),
    }
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    intrinsic: &AggregateIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::intrinsics::aggregate(context, intrinsic, result, span),
        FnContext::Comptime(_) => comptime::intrinsics::aggregate(context, intrinsic, result, span),
    }
}

pub fn internal(
    context: &mut LocalLoweringContext,
    intrinsic: &InternalIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::intrinsics::internal(context, intrinsic, result, span),
        FnContext::Comptime(_) => comptime::intrinsics::internal(context, intrinsic, result, span),
    }
}

pub fn va(
    context: &mut LocalLoweringContext,
    intrinsic: &VAIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::intrinsics::va(context, intrinsic, result, span),
        FnContext::Comptime(_) => comptime::intrinsics::va(context, intrinsic, result, span),
    }
}
