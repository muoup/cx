// Might have gone a bit overboard removing the old emit.rs, we should however make this thinner and just for helpers;
// ensure also that we call this via emit::method_name rather than naming it emit_method_name
//
// Avoid any routines in here that are hyper-specific and would only feasibly be used in one place

pub mod comptime;
mod storage;

pub use storage::{
    adopt, allocate, bind_local, copy, initialize, invalidate, lift, materialize, scratch,
    temporary, write,
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
    log::log_hmir_error,
    lower::expr,
    value::{
        Bindings, HMIRValue, HMIRValueKind,
        address::{Addressable, HMIRAddress},
        constant::HMIRConstant,
    },
};

fn unsupported<T>(span: &TokenRange, operation: &str) -> CXResult<T> {
    log_hmir_error(
        span,
        &catalogue::mir::COMPTIME_INVALID_OPERATION,
        operation.into(),
    )
}

pub fn open_scope(context: &mut LocalLoweringContext, span: &TokenRange) -> usize {
    match context.context_mut() {
        FnContext::Runtime(_) => expr::open_scope(context, span),
        FnContext::Comptime(comptime) => {
            let scopes = comptime.scopes_mut();
            let parent = scopes.top();
            scopes.push(ComptimeScope::default(), Some(parent), Bindings::default())
        }
    }
}

pub fn close_scope(context: &mut LocalLoweringContext, span: &TokenRange) {
    match context.context_mut() {
        FnContext::Runtime(_) => expr::close_scope(context, span),
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
        HMIRValueKind::Constant(HMIRConstant::Function(_) | HMIRConstant::Curried { .. }) => {
            return match context.context() {
                FnContext::Runtime(_) => expr::function_address(context, value, ty, span),
                FnContext::Comptime(_) => {
                    let function = context.env_mut().canonical_value(value.clone());
                    Ok(HMIRValue::constant(
                        ty,
                        HMIRConstant::Address(HMIRAddress::new(
                            Addressable::Function(Box::new(function)),
                            0,
                        )),
                    ))
                }
            };
        }
        HMIRValueKind::Constant(HMIRConstant::Str(text)) => {
            let address = HMIRAddress::new(Addressable::Literal(text.clone()), 0);
            return Ok(HMIRValue::constant(ty, HMIRConstant::Address(address)));
        }
        HMIRValueKind::Indirect(pointer) if context.env().pointer_inner(pointer.ty()).is_some() => {
            return Ok(pointer.as_ref().clone().with_ty(ty));
        }
        _ => {}
    }

    match context.context() {
        FnContext::Runtime(_) => expr::storage::address(context, value, ty, span),
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
    expr::storage::null_check(context, base.unwrap_or(&pointer), span)?;

    todo!()
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
        FnContext::Runtime(_) => expr::storage::field(context, base, index, ty, bitfield, span),
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
        FnContext::Runtime(_) => expr::op::aggregate(context, ty, fields, span),
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
        FnContext::Runtime(_) => expr::op::tag(context, value, ty, span),
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
        FnContext::Runtime(_) => expr::op::binary(context, op, lhs, rhs, result, span),
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
        FnContext::Runtime(_) => expr::op::unary(context, op, value, span),
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
        FnContext::Runtime(_) => expr::op::cast(context, value, target, span),
        FnContext::Comptime(_) => comptime::cast(context, value, target, span),
    }
}

pub fn variadic(
    context: &mut LocalLoweringContext,
    intrinsic: &VAIntrinsic<HMIRValue, HMIRTypeID>,
    span: &TokenRange,
) -> CXResult<Option<HMIRValue>> {
    match context.context() {
        FnContext::Runtime(_) => expr::variadic(context, intrinsic, span),
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
        FnContext::Runtime(_) => expr::call(context, callee, args, result, span),
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
        FnContext::Runtime(_) => expr::op::offset(context, pointer, index, element, subtract, span),
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
        FnContext::Runtime(_) => expr::op::difference(context, lhs, rhs, element, result, span),
        FnContext::Comptime(_) => comptime::difference(context, lhs, rhs, element, result, span),
    }
}
