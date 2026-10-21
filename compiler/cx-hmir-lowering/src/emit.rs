// Might have gone a bit overboard removing the old emit.rs, we should however make this thinner and just for helpers;
// ensure also that we call this via emit::method_name rather than naming it emit_method_name
//
// Avoid any routines in here that are hyper-specific and would only feasibly be used in one place

mod comptime;

pub mod storage;

pub(crate) mod runtime;

pub use storage::{allocate, copy, initialize, invalidate, lift, scratch, write};

use cx_hmir::{HMIRBinaryOp, HMIRUnaryOp, ty::HMIRTypeID};
use cx_log::{CXResult, catalogue::mir};
use cx_tokens::TokenRange;

use crate::{
    env::function::{FnContext, LocalLoweringContext},
    eval::types,
    log::log_hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

fn unsupported<T>(span: &TokenRange, operation: &str) -> CXResult<T> {
    log_hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, operation.into())
}

pub fn open_scope(context: &mut LocalLoweringContext, span: TokenRange) {
    match context.context() {
        FnContext::Runtime(_) => runtime::open_scope(context, span),
        FnContext::Comptime(_) => {}
    }
}

pub fn close_scope(context: &mut LocalLoweringContext, span: TokenRange) {
    match context.context() {
        FnContext::Runtime(_) => runtime::close_scope(context, span),
        FnContext::Comptime(_) => {}
    }
}

pub fn reference(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = types::reference(context.env_mut(), value.ty(), span);
    address(context, value, ty, span)
}

pub fn address(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::address(context, value, ty, span),
        FnContext::Comptime(_) => Ok(comptime::address(value, ty)),
    }
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if matches!(
        pointer.kind(),
        HMIRValueKind::Constant(HMIRConstant::Address { .. })
    ) {
        return Ok(HMIRValue::indirect(inner, pointer));
    }

    match context.context() {
        FnContext::Runtime(_) => runtime::dereference(context, pointer, inner, span),
        FnContext::Comptime(_) => comptime::dereference(context, pointer, inner, span),
    }
}

pub fn field(
    context: &mut LocalLoweringContext,
    base: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::field(context, base, index, ty, span),
        FnContext::Comptime(_) => unsupported(span, "member access"),
    }
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::aggregate(context, ty, fields, span),
        FnContext::Comptime(_) => comptime::aggregate(context, ty, fields, span),
    }
}

pub fn binary(
    context: &mut LocalLoweringContext,
    op: HMIRBinaryOp,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::binary(context, op, lhs, rhs, result, span),
        FnContext::Comptime(_) => comptime::binary(context, op, lhs, rhs, result, span),
    }
}

pub fn unary(
    context: &mut LocalLoweringContext,
    op: HMIRUnaryOp,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::unary(context, op, value, span),
        FnContext::Comptime(_) => comptime::unary(context, op, value, span),
    }
}

pub fn cast(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::cast(context, value, target, span),
        FnContext::Comptime(_) => comptime::cast(context, value, target, span),
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
        FnContext::Runtime(_) => runtime::call(context, callee, args, result, span),
        FnContext::Comptime(_) => unsupported(span, "a function call"),
    }
}

pub fn offset(
    context: &mut LocalLoweringContext,
    pointer: &HMIRValue,
    index: &HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::offset(context, pointer, index, element, subtract, span),
        FnContext::Comptime(_) => unsupported(span, "pointer arithmetic"),
    }
}

pub fn difference(
    context: &mut LocalLoweringContext,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    element: HMIRTypeID,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::difference(context, lhs, rhs, element, result, span),
        FnContext::Comptime(_) => unsupported(span, "pointer arithmetic"),
    }
}
