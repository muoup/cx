use cx_hmir::{
    expr::constant::HMIRConstant,
    ty::HMIRTypeID,
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
};
use cx_log::{CXResult, catalogue::mir};
use cx_tokens::TokenRange;

use crate::{
    env::function::{
        ComptimeContext, FnContext, LocalLoweringContext, comptime::ComptimeSlotState,
    },
    log::hmir_error,
};

fn comptime<'a>(context: &'a mut LocalLoweringContext<'_, '_, '_>) -> &'a mut ComptimeContext {
    match context.context_mut() {
        FnContext::Comptime(comptime) => comptime,
        FnContext::Runtime(_) => unreachable!("comptime evaluation in a runtime context"),
    }
}

pub fn allocate(context: &mut LocalLoweringContext, ty: HMIRTypeID) -> HMIRValue {
    comptime(context).allocate(ty)
}

pub fn read(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let slot = match value.kind() {
        HMIRValueKind::Constant(_) => return Ok(value),
        HMIRValueKind::Place(HMIRPlace::Local(slot)) => *slot,
        HMIRValueKind::Place(HMIRPlace::Global(_)) => todo!("comptime read of a global"),
        HMIRValueKind::Indirect(_) => todo!("comptime read through a subobject address"),
    };

    match comptime(context).slot(slot).state() {
        ComptimeSlotState::Value(constant) => Ok(HMIRValue::constant(value.ty(), constant.clone())),
        ComptimeSlotState::Uninitialized => Err(hmir_error(
            span,
            &mir::COMPTIME_UNSET,
            "uninitialized value".into(),
        )),
        ComptimeSlotState::Invalidated => {
            Err(hmir_error(span, &mir::COMPTIME_UNSET, "moved value".into()))
        }
    }
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    let value = read(context, value, span)?;
    let HMIRValueKind::Constant(constant) = value.kind() else {
        unreachable!("a comptime read yields a constant");
    };

    match target.kind() {
        HMIRValueKind::Place(HMIRPlace::Local(slot)) => {
            *comptime(context).slot_mut(*slot).state_mut() =
                ComptimeSlotState::Value(constant.clone());
        }
        HMIRValueKind::Place(HMIRPlace::Global(_)) => todo!("comptime write to a global"),
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
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointer = read(context, pointer, span)?;
    if !matches!(
        pointer.kind(),
        HMIRValueKind::Constant(HMIRConstant::Address { .. })
    ) {
        return Err(hmir_error(
            span,
            &mir::EXPECTED_CONSTANT,
            ("pointer".into(), "address".into()),
        ));
    }

    Ok(HMIRValue::indirect(inner, pointer))
}
