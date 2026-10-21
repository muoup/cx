use cx_hmir::ty::HMIRTypeID;
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
fn tracked_slot(value: &HMIRValue) -> Option<HMIRSlotID> {
    match value.kind() {
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => Some(*slot),
        HMIRValueKind::PlaceRef(HMIRPlace::Global(_)) => todo!("comptime global storage"),
        HMIRValueKind::Constant(_) | HMIRValueKind::Indirect(_) => None,
    }
}

pub fn copy(context: &mut LocalLoweringContext, value: HMIRValue) -> CXResult<HMIRValue> {
    let slot = match value.kind() {
        HMIRValueKind::Constant(_) => return Ok(value),
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => *slot,
        HMIRValueKind::PlaceRef(HMIRPlace::Global(_)) => todo!("comptime read of a global"),
        HMIRValueKind::Indirect(_) => todo!("comptime read through a subobject address"),
    };

    match context.comptime_mut().slot(slot).state() {
        ComptimeSlotState::Value(constant) => Ok(HMIRValue::constant(value.ty(), constant.clone())),
        ComptimeSlotState::Uninitialized | ComptimeSlotState::Invalidated => {
            todo!("diagnostic: comptime read of a slot that holds no value")
        }
    }
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
) -> CXResult<()> {
    let value = copy(context, value.clone())?;
    let HMIRValueKind::Constant(constant) = value.kind() else {
        unreachable!("a comptime read yields a constant");
    };

    match target.kind() {
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => {
            *context.comptime_mut().slot_mut(*slot).state_mut() =
                ComptimeSlotState::Value(constant.clone());
        }
        HMIRValueKind::PlaceRef(HMIRPlace::Global(_)) => todo!("comptime write to a global"),
        HMIRValueKind::Indirect(_) => todo!("comptime write through a subobject address"),
        HMIRValueKind::Constant(_) => unreachable!("write to a value without storage"),
    }

    Ok(())
}

pub fn initialize(context: &mut LocalLoweringContext, value: &HMIRValue) {
    if let Some(slot) = tracked_slot(value) {
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
    if let Some(slot) = tracked_slot(value) {
        *context.comptime_mut().slot_mut(slot).state_mut() = match kind {
            MIRInvalidationKind::Drop => ComptimeSlotState::Uninitialized,
            MIRInvalidationKind::Move | MIRInvalidationKind::Leak => ComptimeSlotState::Invalidated,
        };
    }
}

pub fn address(pointer: &HMIRValue, ty: HMIRTypeID) -> HMIRValue {
    match pointer.kind() {
        HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => HMIRValue::constant(
            ty,
            HMIRConstant::Address {
                place: *place,
                path: path.clone(),
                ty,
            },
        ),
        _ => todo!("comptime address through a pointer that holds no address"),
    }
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
) -> CXResult<HMIRValue> {
    let pointer = copy(context, pointer)?;
    if !matches!(
        pointer.kind(),
        HMIRValueKind::Constant(HMIRConstant::Address { .. })
    ) {
        todo!("diagnostic: comptime dereference of a pointer that holds no address")
    }

    Ok(HMIRValue::indirect(inner, pointer))
}
