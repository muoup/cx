use cx_hmir::{
    body::HMIRLocalID,
    expr::constant::HMIRConstant,
    ty::HMIRTypeID,
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
};
use cx_log::CXResult;

use crate::env::function::{LocalLoweringContext, comptime::ComptimeSlotState};

pub fn bind_local(
    context: &mut LocalLoweringContext,
    local: HMIRLocalID,
    value: HMIRValue,
) -> CXResult<()> {
    context.comptime_mut().set_local(local, value);
    Ok(())
}

pub fn allocate_slot(context: &mut LocalLoweringContext, ty: HMIRTypeID) -> HMIRPlace {
    context.comptime_mut().allocate(ty)
}

pub fn read(context: &mut LocalLoweringContext, value: HMIRValue) -> CXResult<HMIRValue> {
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
    value: HMIRValue,
) -> CXResult<()> {
    let value = read(context, value)?;
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
    let pointer = read(context, pointer)?;
    if !matches!(
        pointer.kind(),
        HMIRValueKind::Constant(HMIRConstant::Address { .. })
    ) {
        todo!("diagnostic: comptime dereference of a pointer that holds no address")
    }

    Ok(HMIRValue::indirect(inner, pointer))
}
