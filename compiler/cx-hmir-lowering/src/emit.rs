// Might have gone a bit overboard removing the old emit.rs, we should however make this thinner and just for helpers;
// ensure also that we call this via emit::method_name rather than naming it emit_method_name
//
// Avoid any routines in here that are hyper-specific and would only feasibly be used in one place

mod comptime;
pub(crate) mod runtime;

use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    expr::constant::HMIRConstant,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
    },
    value::{HMIRValue, HMIRValueKind},
};
use cx_log::CXResult;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::env::function::{FnContext, LocalLoweringContext};

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

pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    nodrop: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::allocate(context, ty, name, nodrop, span),
        FnContext::Comptime(_) => Ok(comptime::allocate(context, ty)),
    }
}

pub fn addressable(context: &mut LocalLoweringContext, value: &HMIRValue) -> bool {
    match context.context() {
        FnContext::Runtime(_) => runtime::addressable(context, value),
        FnContext::Comptime(_) => !matches!(value.kind(), HMIRValueKind::Constant(_)),
    }
}

pub fn read(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::read(context, value, span),
        FnContext::Comptime(_) => comptime::read(context, value, span),
    }
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: HMIRValue,
    initialize: bool,
    span: &TokenRange,
) -> CXResult<()> {
    match context.context() {
        FnContext::Runtime(_) => runtime::write(context, target, value, initialize, span),
        FnContext::Comptime(_) => comptime::write(context, target, value, span),
    }
}

pub fn spill(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if addressable(context, &value) {
        return Ok(value);
    }

    let place = allocate(context, value.ty(), None, false, span)?;
    write(context, &place, value, true, span)?;

    Ok(place)
}

pub fn referent(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        return Ok(value);
    };

    let reference = read(context, value, span)?;
    Ok(HMIRValue::indirect(inner, reference))
}

pub fn reference(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::ReferenceTo(value.ty()),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));
    address(context, value, ty, span)
}

pub fn address(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match value.kind() {
        HMIRValueKind::Place(place) => Ok(HMIRValue::constant(
            ty,
            HMIRConstant::Address {
                place: *place,
                path: Vec::new(),
                ty,
            },
        )),
        HMIRValueKind::Indirect(pointer) => match context.context() {
            FnContext::Runtime(_) => runtime::address(context, pointer, ty, span),
            FnContext::Comptime(_) => Ok(comptime::address(pointer, ty)),
        },
        HMIRValueKind::Constant(_) => unreachable!("only storage has an address"),
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
        FnContext::Comptime(_) => todo!("comptime member access"),
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
        FnContext::Comptime(_) => todo!("comptime aggregate initialization"),
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
        FnContext::Comptime(_) => todo!("comptime folding of binary operators"),
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
        FnContext::Comptime(_) => todo!("comptime folding of unary operators"),
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
        FnContext::Comptime(_) => todo!("comptime folding of conversions"),
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
        FnContext::Comptime(_) => todo!("comptime calls"),
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
        FnContext::Comptime(_) => todo!("comptime pointer arithmetic"),
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
        FnContext::Comptime(_) => todo!("comptime pointer arithmetic"),
    }
}
