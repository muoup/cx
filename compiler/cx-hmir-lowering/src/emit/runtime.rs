pub mod flow;
pub mod intrinsics;
pub mod storage;

use cx_hmir::{
    HMIRBinaryOp,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_intrinsics::{
    IntBinaryOp, IntIntrinsic, InternalIntrinsic, PointerCompareOp, PointerIntrinsic,
};
use cx_log::CXResult;
use cx_mir::{
    MIRConstant, MIRInstructionKind, MIRInternalIntrinsic, MIRPlaceDecl, MIRPlaceID, MIRRegister,
    MIRRegisterDecl, MIRValue, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    emit::runtime,
    env::function::{LocalLoweringContext, runtime::RuntimeScope, scope::Bindings},
    eval::types,
    lower::types::{lower_type, mir_float, mir_int},
    value::{
        HMIRPlaceID, HMIRRegisterID, HMIRValIntrinsic, HMIRValue, HMIRValueKind,
        immediate::HMIRImmediate,
    },
};

pub fn open_scope(context: &mut LocalLoweringContext, span: &TokenRange) -> usize {
    let runtime = context.runtime_mut();
    let scopes = runtime.scopes_mut();
    let parent = scopes.top();

    scopes.push(RuntimeScope::new(false), Some(parent), Bindings::default())
}

pub fn close_scope(context: &mut LocalLoweringContext, span: &TokenRange) {
    let index = context.runtime().scopes().top();
    drop_scope(context, index, span);
    context.runtime_mut().scopes_mut().pop();
}

pub fn drop_scope(context: &mut LocalLoweringContext, index: usize, span: &TokenRange) {
    let Some(scope) = context.runtime().scopes().control(index).mir() else {
        return;
    };

    let places = context
        .runtime()
        .builder()
        .places()
        .iter()
        .enumerate()
        .filter(|(_, place)| place.scope() == scope)
        .map(|(index, _)| MIRPlaceID::new(index))
        .collect::<Vec<_>>();

    for place in places.into_iter().rev() {
        let invalidate = MIRInstructionKind::Invalidate {
            place,
            kind: MIRInvalidationKind::Drop,
        };
        context.runtime_mut().emit(invalidate, span.clone());
    }
}

pub fn value(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    Ok(match value.kind() {
        HMIRValueKind::Immediate(constant) => {
            MIRValue::Constant(self::constant(context, constant, value.ty(), span)?)
        }
        _ => todo!(),
    })
}

pub fn constant(
    context: &mut LocalLoweringContext,
    constant: &HMIRImmediate,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRConstant> {
    Ok(
        match (constant, context.env_mut().resolve_kind(ty).clone()) {
            (HMIRImmediate::Unit, _) => MIRConstant::Unit,
            (HMIRImmediate::Int(value), HMIRTypeKind::Int(int)) => MIRConstant::Integer {
                ty: mir_int(int.width()),
                value: *value,
            },
            (HMIRImmediate::Float(value), HMIRTypeKind::Float(float)) => MIRConstant::Float {
                value: *value,
                ty: mir_float(float.width()),
            },
            _ => todo!(),
        },
    )
}

pub(crate) fn register(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRRegisterID> {
    let ty = lower_type(context.env_mut(), ty, span)?;
    let runtime = context.runtime_mut();

    let mir_register = runtime
        .builder_mut()
        .allocate_register(MIRRegisterDecl::new(ty, None));

    Ok(HMIRRegisterID::new(mir_register.0))
}

pub(crate) fn place(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    nodrop: bool,
    adopted: bool,
    span: &TokenRange,
) -> CXResult<HMIRPlaceID> {
    let ty = lower_type(context.env_mut(), ty, span)?;
    let runtime = context.runtime_mut();

    let mir_place = runtime
        .builder_mut()
        .allocate_place(MIRPlaceDecl::new(ty, None, nodrop, adopted));

    Ok(HMIRPlaceID::new(mir_place.0))
}
