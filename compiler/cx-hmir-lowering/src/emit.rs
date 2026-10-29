// Might have gone a bit overboard removing the old emit.rs, we should however make this thinner and just for helpers;
// ensure also that we call this via emit::method_name rather than naming it emit_method_name
//
// Avoid any routines in here that are hyper-specific and would only feasibly be used in one place

mod comptime;
mod storage;

pub use storage::{
    adopt, allocate, bind_local, copy, initialize, invalidate, lift, scratch, temporary, write,
};

use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    ty::{HMIRTypeID, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_intrinsics::VAIntrinsic;
use cx_log::{CXResult, catalogue};
use cx_tokens::TokenRange;

use crate::{
    env::function::{FnContext, LocalLoweringContext, comptime::ComptimeScope, memory::placement},
    eval::types,
    lower::mir,
    log::log_hmir_error,
    value::{Bindings, HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

fn unsupported<T>(span: &TokenRange, operation: &str) -> CXResult<T> {
    log_hmir_error(span, &catalogue::mir::COMPTIME_INVALID_OPERATION, operation.into())
}

pub fn open_scope(context: &mut LocalLoweringContext, span: &TokenRange) -> usize {
    match context.context_mut() {
        FnContext::Runtime(_) => mir::open_scope(context, span),
        FnContext::Comptime(comptime) => {
            let scopes = comptime.scopes_mut();
            let parent = scopes.top();
            scopes.push(ComptimeScope::default(), Some(parent), Bindings::new())
        }
    }
}

pub fn close_scope(context: &mut LocalLoweringContext, span: &TokenRange) {
    match context.context_mut() {
        FnContext::Runtime(_) => mir::close_scope(context, span),
        FnContext::Comptime(comptime) => {
            comptime.scopes_mut().pop();
        }
    }
}

pub fn reference(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let bitfield = match value.kind() {
        HMIRValueKind::Indirect(_) => types::bitfield(context.env(), value),
        _ => None,
    };
    let ty = types::reference(context.env_mut(), value.ty(), bitfield, span);
    match value.kind() {
        HMIRValueKind::Indirect(pointer) if context.env().same_type(pointer.ty(), ty) => {
            Ok(pointer.as_ref().clone())
        }
        _ => address(context, value, ty, span),
    }
}

pub fn address(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Function(_) | HMIRConstant::Curried { .. } | HMIRConstant::Str(_)) => {
            return Ok(value.clone().with_ty(ty));
        }
        HMIRValueKind::Indirect(pointer) if context.env().pointer_inner(pointer.ty()).is_some() => {
            return Ok(pointer.as_ref().clone().with_ty(ty));
        }
        _ => {}
    }

    match context.context() {
        FnContext::Runtime(_) => mir::address(context, value, ty, span),
        FnContext::Comptime(_) => comptime::address(context, value, ty, span),
    }
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
    base: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match (pointer.kind(), context.context()) {
        (HMIRValueKind::Constant(HMIRConstant::Address(_)), _) => Ok(HMIRValue::indirect(inner, pointer)),
        (_, FnContext::Runtime(_)) => mir::dereference(context, pointer, inner, base, span),
        (_, FnContext::Comptime(_)) => comptime::dereference(context, pointer, inner, span),
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
        FnContext::Runtime(_) => mir::field(context, base, index, ty, bitfield, span),
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
        FnContext::Runtime(_) => mir::aggregate(context, ty, fields, span),
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
        FnContext::Runtime(_) => mir::tag(context, value, ty, span),
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
        FnContext::Runtime(_) => mir::binary(context, op, lhs, rhs, result, span),
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
        FnContext::Runtime(_) => mir::unary(context, op, value, span),
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
        FnContext::Runtime(_) => mir::cast(context, value, target, span),
        FnContext::Comptime(_) => comptime::cast(context, value, target, span),
    }
}

pub fn variadic(
    context: &mut LocalLoweringContext,
    intrinsic: &VAIntrinsic<HMIRValue, HMIRTypeID>,
    span: &TokenRange,
) -> CXResult<Option<HMIRValue>> {
    match context.context() {
        FnContext::Runtime(_) => mir::variadic(context, intrinsic, span),
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
        FnContext::Runtime(_) => mir::call(context, callee, args, result, span),
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
        FnContext::Runtime(_) => mir::offset(context, pointer, index, element, subtract, span),
        FnContext::Comptime(_) => comptime::offset(context, pointer, index, element, subtract, span),
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
        FnContext::Runtime(_) => mir::difference(context, lhs, rhs, element, result, span),
        FnContext::Comptime(_) => comptime::difference(context, lhs, rhs, element, result, span),
    }
}
