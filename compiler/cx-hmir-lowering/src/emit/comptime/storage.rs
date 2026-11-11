use cx_hmir::ty::{HMIRTypeID, context::HMIRTypeContext};
use cx_log::{
    CXResult,
    catalogue::{analysis, mir},
};
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::function::{LocalLoweringContext, comptime::ComptimeSlotState, memory::layout},
    eval::types,
    log::hmir_error,
    value::{
        HMIRValue, HMIRValueKind, address::HMIRAddress, immediate::HMIRImmediate,
        place::HMIRPlaceID,
    },
};

pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let place = context.allocate(ty, span)?;
    context.comptime_mut().declare_slot(place, ty, name);
    let reference = types::reference(context.env_mut(), ty, None, span);

    Ok(HMIRValue::new(reference, HMIRAddress::new(place, 0)))
}

fn locate(
    context: &mut LocalLoweringContext,
    reference: &HMIRValue,
    write: bool,
    span: &TokenRange,
) -> CXResult<HMIRAddress<HMIRPlaceID>> {
    let location = context.locate(reference.kind(), write, span)?;
    live(context, *location.base(), write, span)?;

    Ok(location)
}

// The slot a reference covers whole, which tracks the liveness of what it holds
fn tracked(
    context: &mut LocalLoweringContext,
    reference: &HMIRValue,
    span: &TokenRange,
) -> CXResult<Option<HMIRPlaceID>> {
    let HMIRValueKind::Reference(address) = reference.kind() else {
        return Ok(None);
    };
    let place = *address.base();
    let Some(slot) = context.comptime().slot(place).map(|slot| slot.ty()) else {
        return Ok(None);
    };
    if address.offset() != 0 || types::bitfield(context.env(), reference.ty()).is_some() {
        return Ok(None);
    }
    let ty = context.env().referent(reference.ty());
    let whole = layout(context.env_mut(), ty, span)? == layout(context.env_mut(), slot, span)?;

    Ok(whole.then_some(place))
}

fn live(
    context: &LocalLoweringContext,
    place: HMIRPlaceID,
    writing: bool,
    span: &TokenRange,
) -> CXResult<()> {
    let Some(slot) = context.comptime().slot(place) else {
        return Ok(());
    };
    let subject = match slot.name() {
        Some(name) => format!("'{name}'"),
        None => "this temporary".into(),
    };
    match slot.state() {
        ComptimeSlotState::Live => Ok(()),
        ComptimeSlotState::Uninitialized if writing => Ok(()),
        ComptimeSlotState::Uninitialized => Err(hmir_error(span, &mir::COMPTIME_UNSET, subject)),
        ComptimeSlotState::Invalidated => {
            let function = context.comptime().function().to_string();
            let place = slot
                .name()
                .map_or_else(|| "temporary".into(), |name| name.to_string());
            let discarded = place == "_";
            let operation = match writing {
                true => "is written",
                false => "is used",
            };
            let args = (function, place, operation.into(), discarded);
            Err(hmir_error(span, &analysis::AFTER_MOVE, args))
        }
    }
}

// The contents of the storage a reference designates in the form they have outside of comptime storage, which
// fails where they hold an address of it
pub fn contents(
    context: &mut LocalLoweringContext,
    reference: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let location = locate(context, reference, false, span)?;
    let inner = context.env().referent(reference.ty());
    let bitfield = types::bitfield(context.env(), reference.ty());

    Ok(HMIRValue::new(inner, context.load(&location, inner, bitfield, span)?))
}

// The contents of the storage a reference designates, as they are now. An aggregate is copied into storage of its
// own, untracked as it binds nothing, since its members may hold addresses of comptime storage
pub fn read(
    context: &mut LocalLoweringContext,
    source: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = context.env().referent(source.ty());
    if !types::is_aggregate(context.env(), ty) {
        return contents(context, source, span);
    }
    let source = locate(context, source, false, span)?;
    let copy = HMIRAddress::new(context.allocate(ty, span)?, 0);
    context.copy(&copy, &source, ty, span)?;

    Ok(HMIRValue::new(ty, copy))
}

// The variant of the tagged union 'value' holds, whether it is a constant or held in storage
pub fn variant(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<usize> {
    if let HMIRValueKind::Immediate(HMIRImmediate::Union { variant, .. }) = value.kind() {
        return Ok(*variant);
    }
    let location = locate(context, value, false, span)?;
    let ty = context.env().referent(value.ty());
    context.variant(&location, ty, span)
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    let tracked = tracked(context, target, span)?;
    let location = match tracked {
        Some(_) => context.locate(target.kind(), true, span)?,
        None => locate(context, target, true, span)?,
    };
    let ty = context.env().referent(target.ty());
    let bitfield = types::bitfield(context.env(), target.ty());
    match is_held(context, value) {
        true => {
            let source = locate(context, value, false, span)?;
            context.copy(&location, &source, ty, span)?;
        }
        false => context.store(&location, ty, value.kind(), bitfield, span)?,
    }
    if let Some(slot) = context.comptime_mut().slot_mut(*location.base()) {
        slot.set_state(ComptimeSlotState::Live);
    }

    Ok(())
}

// Whether 'value' is held in comptime storage rather than an address of it, as an aggregate read out of it is
fn is_held(context: &LocalLoweringContext, value: &HMIRValue) -> bool {
    matches!(value.kind(), HMIRValueKind::Reference(_))
        && !context.env().is_reference(value.ty())
        && !context.env().is_pointer(value.ty())
}

fn mark(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    state: ComptimeSlotState,
    span: &TokenRange,
) {
    if let Ok(Some(place)) = tracked(context, target, span)
        && let Some(slot) = context.comptime_mut().slot_mut(place)
    {
        slot.set_state(state);
    }
}

pub fn initialize(context: &mut LocalLoweringContext, target: &HMIRValue, span: &TokenRange) {
    mark(context, target, ComptimeSlotState::Live, span);
}

pub fn invalidate(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    kind: MIRInvalidationKind,
    span: &TokenRange,
) {
    let state = match kind {
        MIRInvalidationKind::Drop => ComptimeSlotState::Uninitialized,
        MIRInvalidationKind::Move | MIRInvalidationKind::Leak => ComptimeSlotState::Invalidated,
    };
    mark(context, target, state, span);
}
