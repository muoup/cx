use cx_hmir::expr::constant::HMIRConstant;
use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind};
use cx_log::CXResult;
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRInstructionKind, MIRIntrinsic, MIRRegister,
    MIRRegisterDecl, MIRScopeID, MIRTarget, MIRValue, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::lowering::FnLoweringContext,
    lower::{HMIRValue, HMIRValueKind, shape::{TypeShape, shape}},
};

pub(crate) fn new_register(
    cx: &mut FnLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRRegister> {
    let mir = cx.env().lower_type(ty, span)?;
    Ok(cx.builder().allocate_register(MIRRegisterDecl::new(mir, None)))
}

pub(crate) fn new_block(cx: &mut FnLoweringContext, name: &str) -> MIRBasicBlockID {
    cx.builder().add_block(Some(CXIdent::new(name)))
}

pub(crate) fn new_block_param(
    cx: &mut FnLoweringContext,
    block: MIRBasicBlockID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let register = new_register(cx, ty, span)?;
    cx.builder()
        .block_mut(block)
        .expect("block belongs to the function")
        .push_param(register);

    Ok(HMIRValue::register(ty, register))
}

pub(crate) fn emit_intrinsic(
    cx: &mut FnLoweringContext,
    intrinsic: impl Into<MIRIntrinsic>,
    span: &TokenRange,
) {
    cx.builder()
        .emit(MIRInstructionKind::IntrinsicOp(intrinsic.into()), span);
}

pub(crate) fn emit_jump(
    cx: &mut FnLoweringContext,
    block: MIRBasicBlockID,
    args: Vec<MIRValue>,
    span: &TokenRange,
) {
    let target = MIRBlockTarget::with_args(block, args);
    cx.builder().emit(MIRInstructionKind::Jump { target }, span);
}

pub(crate) fn emit_branch(
    cx: &mut FnLoweringContext,
    cond: MIRValue,
    true_target: MIRBlockTarget,
    false_target: MIRBlockTarget,
    span: &TokenRange,
) {
    cx.builder().emit(
        MIRInstructionKind::Branch {
            cond,
            true_target,
            false_target,
        },
        span,
    );
}

pub(crate) fn emit_return(cx: &mut FnLoweringContext, value: Option<MIRValue>, span: &TokenRange) {
    let current = cx.builder().current_scope();
    drop_scope(cx, current, span);

    let root = MIRScopeID::new(0);
    if current != root {
        drop_scope(cx, root, span);
    }

    cx.builder().emit(MIRInstructionKind::Return { value }, span);
}

pub(crate) fn drop_scope(cx: &mut FnLoweringContext, scope: MIRScopeID, span: &TokenRange) {
    let places = cx
        .builder()
        .places()
        .iter()
        .enumerate()
        .filter(|(_, place)| place.scope() == scope)
        .map(|(index, _)| cx_mir::MIRPlaceID::new(index))
        .collect::<Vec<_>>();

    for place in places.into_iter().rev() {
        cx.builder().emit(
            MIRInstructionKind::Invalidate {
                place: MIRBindable::Place(place),
                kind: MIRInvalidationKind::Drop,
            },
            span,
        );
    }
}

pub(crate) fn void_value(cx: &mut FnLoweringContext, span: &TokenRange) -> HMIRValue {
    let ty = cx.env().realize_type(HMIRTypeKind::Void, span);
    HMIRValue::constant(ty, HMIRConstant::Unit)
}

pub(crate) fn unreachable_value(cx: &mut FnLoweringContext, span: &TokenRange) -> HMIRValue {
    let ty = cx.env().realize_type(HMIRTypeKind::Unreachable, span);
    HMIRValue::constant(ty, HMIRConstant::Unit)
}

pub(crate) fn diverges(cx: &mut FnLoweringContext, value: &HMIRValue) -> bool {
    shape(cx, value.ty()) == TypeShape::Unreachable
}

pub(crate) fn rvalue(
    cx: &mut FnLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let HMIRValueKind::Place(place) = value.kind() else {
        return Ok(value);
    };

    let ty = value.ty();
    let mir = cx.env().lower_type(ty, span)?;
    let out = new_register(cx, ty, span)?;

    cx.builder().emit(
        MIRInstructionKind::Store {
            target: MIRTarget::Register(out),
            value: MIRValue::PlaceRef(*place),
            ty: mir,
            bitfield: None,
        },
        span,
    );

    Ok(HMIRValue::register(ty, out))
}
