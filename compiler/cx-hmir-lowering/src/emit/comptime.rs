use cx_hmir::ty::{HMIRTypeID, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_mir::expr::instruction::MIRInvalidationKind;

use crate::{
    env::function::{LocalLoweringContext, comptime::ComptimeSlotState},
    value::{HMIRPlace, HMIRSlotID, HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub fn allocate(context: &mut LocalLoweringContext, ty: HMIRTypeID) -> HMIRValue {
    let slot = context.comptime_mut().allocate_slot(ty);

    HMIRValue::place(ty, HMIRPlace::Local(slot))
}

// The slot whose state a value's liveness is recorded on; storage behind an address is untracked
fn tracked_slot(context: &LocalLoweringContext, value: &HMIRValue) -> Option<HMIRSlotID> {
    match value.kind() {
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => Some(*slot),
        HMIRValueKind::PlaceRef(HMIRPlace::Global(_)) => todo!("comptime global storage"),
        HMIRValueKind::Constant(HMIRConstant::Address {
            place: HMIRPlace::Local(slot),
            path,
            ..
        }) if path.is_empty() && context.env().is_reference(value.ty()) => Some(*slot),
        HMIRValueKind::Constant(_) | HMIRValueKind::Indirect(_) => None,
    }
}

fn read(context: &LocalLoweringContext, value: &HMIRValue) -> CXResult<HMIRConstant> {
    let slot = match (value.kind(), tracked_slot(context, value)) {
        (_, Some(slot)) => slot,
        (HMIRValueKind::Constant(constant), None)
            if !matches!(constant, HMIRConstant::Address { .. })
                || !context.env().is_reference(value.ty()) =>
        {
            return Ok(constant.clone());
        }
        _ => todo!("comptime read through a subobject address"),
    };

    match context.comptime().slot(slot).state() {
        ComptimeSlotState::Value(constant) => Ok(constant.clone()),
        ComptimeSlotState::Uninitialized | ComptimeSlotState::Invalidated => {
            todo!("diagnostic: comptime read of a slot that holds no value")
        }
    }
}

pub fn copy(context: &mut LocalLoweringContext, value: HMIRValue) -> CXResult<HMIRValue> {
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        unreachable!("a comptime copy reads from a reference")
    };

    Ok(HMIRValue::constant(inner, read(context, &value)?))
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
) -> CXResult<()> {
    let constant = read(context, value)?;
    let Some(slot) = tracked_slot(context, target) else {
        todo!("comptime write through a subobject address")
    };
    *context.comptime_mut().slot_mut(slot).state_mut() = ComptimeSlotState::Value(constant);

    Ok(())
}

pub fn initialize(context: &mut LocalLoweringContext, value: &HMIRValue) {
    if let Some(slot) = tracked_slot(context, value) {
        debug_assert!(
            matches!(
                context.comptime().slot(slot).state(),
                ComptimeSlotState::Value(_)
            ),
            "comptime slot initialized before it was written"
        );
    }
}

pub fn invalidate(context: &mut LocalLoweringContext, value: &HMIRValue, kind: MIRInvalidationKind) {
    if let Some(slot) = tracked_slot(context, value) {
        *context.comptime_mut().slot_mut(slot).state_mut() = match kind {
            MIRInvalidationKind::Drop => ComptimeSlotState::Uninitialized,
            MIRInvalidationKind::Move | MIRInvalidationKind::Leak => ComptimeSlotState::Invalidated,
        };
    }
}

pub fn address(value: &HMIRValue, ty: HMIRTypeID) -> HMIRValue {
    let (place, path) = match value.kind() {
        HMIRValueKind::PlaceRef(place) => (place, Vec::new()),
        HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => (place, path.clone()),
        HMIRValueKind::Indirect(pointer) => match pointer.kind() {
            HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => {
                (place, path.clone())
            }
            _ => todo!("comptime address through a pointer that holds no address"),
        },
        HMIRValueKind::Constant(_) => unreachable!("only storage has an address"),
    };

    HMIRValue::constant(
        ty,
        HMIRConstant::Address {
            place: *place,
            path,
            ty,
        },
    )
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
) -> CXResult<HMIRValue> {
    let pointer = HMIRValue::constant(pointer.ty(), read(context, &pointer)?);
    if !matches!(
        pointer.kind(),
        HMIRValueKind::Constant(HMIRConstant::Address { .. })
    ) {
        todo!("diagnostic: comptime dereference of a pointer that holds no address")
    }

    Ok(HMIRValue::indirect(inner, pointer))
}
