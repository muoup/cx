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
    value::HMIRValue,
};

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
