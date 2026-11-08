pub mod comptime;
pub mod runtime;

pub use storage::{
    adopt, allocate, bind_local, copy, initialize, invalidate, lift, materialize, scratch,
    temporary, write,
};

use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    ty::{HMIRTypeID, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_intrinsics::VAIntrinsic;
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::function::{
        FnContext, LocalLoweringContext, comptime::ComptimeScope, memory::placement,
        scope::Bindings,
    },
    eval::types,
    value::HMIRValue,
};

pub fn open_scope(context: &mut LocalLoweringContext, span: &TokenRange) -> usize {
    match context.context_mut() {
        FnContext::Runtime(_) => runtime::open_scope(context, span),
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

pub fn null_check(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    match context.context_mut() {
        FnContext::Runtime(_) => runtime::null_check(context, value, span),
        FnContext::Comptime(_) => comptime::null_check(context, value, span)
    }
}

pub fn field(
    context: &mut LocalLoweringContext,
    base: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let owner = context.env().referent(base.ty());
    let bitfield = placement(context.env_mut(), owner, index, span)?;
    match context.context() {
        FnContext::Runtime(_) => runtime::storage::field(context, base, index, ty, bitfield, span),
        FnContext::Comptime(_) => comptime::field(context, base, index, ty, bitfield, span),
    }
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::intrinsics::aggregate(context, ty, fields, span),
        FnContext::Comptime(_) => comptime::aggregate(context, ty, fields, span),
    }
}

pub fn tag(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = types::int(context.env_mut(), HMIRIntWidth::I8, false, span);
    match context.context() {
        FnContext::Runtime(_) => runtime::intrinsics::tag(context, value, ty, span),
        FnContext::Comptime(_) => comptime::tag(context, value, ty, span),
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
        FnContext::Runtime(_) => runtime::intrinsics::binary(context, op, lhs, rhs, result, span),
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
        FnContext::Runtime(_) => runtime::intrinsics::unary(context, op, value, span),
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
        FnContext::Runtime(_) => runtime::intrinsics::cast(context, value, target, span),
        FnContext::Comptime(_) => comptime::cast(context, value, target, span),
    }
}

pub fn variadic(
    context: &mut LocalLoweringContext,
    intrinsic: &VAIntrinsic<HMIRValue, HMIRTypeID>,
    span: &TokenRange,
) -> CXResult<Option<HMIRValue>> {
    match context.context() {
        FnContext::Runtime(_) => runtime::intrinsics::variadic(context, intrinsic, span),
        FnContext::Comptime(_) => unsupported(span, intrinsic.path()),
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
        FnContext::Runtime(_) => runtime::intrinsics::call(context, callee, args, result, span),
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
        FnContext::Runtime(_) => runtime::intrinsics::offset(context, pointer, index, element, subtract, span),
        FnContext::Comptime(_) => {
            comptime::offset(context, pointer, index, element, subtract, span)
        }
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
        FnContext::Runtime(_) => runtime::intrinsics::difference(context, lhs, rhs, element, result, span),
        FnContext::Comptime(_) => comptime::difference(context, lhs, rhs, element, result, span),
    }
}
