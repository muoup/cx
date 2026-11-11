pub mod comptime;
pub mod intrinsics;
pub mod runtime;
pub mod storage;

use cx_hmir::ty::HMIRTypeID;
use cx_log::{CXResult, catalogue::mir};
use cx_tokens::TokenRange;

use crate::{
    env::function::{FnContext, LocalLoweringContext, comptime::ComptimeScope, scope::Bindings},
    log::hmir_error,
    value::{HMIRValIntrinsic, HMIRValue},
};

pub(crate) fn unsupported<T>(span: &TokenRange, operation: &str) -> CXResult<T> {
    Err(hmir_error(
        span,
        &mir::COMPTIME_INVALID_OPERATION,
        operation.into(),
    ))
}

pub fn open_scope(context: &mut LocalLoweringContext) -> usize {
    match context.context_mut() {
        FnContext::Runtime(_) => runtime::open_scope(context),
        FnContext::Comptime(comptime) => {
            let scopes = comptime.scopes_mut();
            let parent = scopes.top();
            scopes.push(ComptimeScope::default(), Some(parent), Bindings::default())
        }
    }
}

pub fn close_scope(context: &mut LocalLoweringContext, span: &TokenRange) {
    match context.context_mut() {
        FnContext::Runtime(_) => runtime::close_scope(context, span),
        FnContext::Comptime(comptime) => {
            comptime.scopes_mut().pop();
        }
    }
}

// An intrinsic without a result produces a value of the unit type 'result'
pub fn intrinsic(
    context: &mut LocalLoweringContext,
    intrinsic: &HMIRValIntrinsic,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match intrinsic {
        HMIRValIntrinsic::Int(op) => intrinsics::int(context, op, result, span),
        HMIRValIntrinsic::Float(op) => intrinsics::float(context, op, result, span),
        HMIRValIntrinsic::Pointer(op) => intrinsics::pointer(context, op, result, span),
        HMIRValIntrinsic::Aggregate(op) => intrinsics::aggregate(context, op, result, span),
        HMIRValIntrinsic::Internal(op) => intrinsics::internal(context, op, result, span),
        HMIRValIntrinsic::VA(op) => intrinsics::va(context, op, result, span),
    }
}

pub fn call(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    args: &[HMIRValue],
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::flow::call(context, callee, args, result, span),
        FnContext::Comptime(_) => unsupported(span, "a function call"),
    }
}
