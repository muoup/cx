use chl_hmir::ty::{HMIRTypeID, context::HMIRTypeContext};
use chl_log::{
    CHLResult,
    catalogue::{analysis, mir},
};
use chl_mir::expr::instruction::MIRInvalidationKind;
use chl_tokens::TokenRange;
use chl_util::identifier::CHLIdent;

use crate::{
    env::function::{
        LocalLoweringContext,
        comptime::{ComptimeSlot, ComptimeSlotState},
    },
    eval::{symbol, types},
    log::hmir_error,
    value::{
        HMIRValue, HMIRValueKind, address::HMIRAddress, immediate::HMIRImmValue,
        place::HMIRPlaceID,
    },
};

pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CHLIdent>,
    span: &TokenRange,
) -> CHLResult<HMIRValue> {
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
) -> CHLResult<HMIRAddress<HMIRPlaceID>> {
    let location = context.locate(reference.kind(), write, span)?;
    let operation = if write { "was written" } else { "was read" };
    require_live(context, *location.base(), operation, span)?;

    Ok(location)
}

pub(in crate::emit) fn require_live(
    context: &LocalLoweringContext,
    place: HMIRPlaceID,
    operation: &str,
    span: &TokenRange,
) -> CHLResult<()> {
    let Some(slot) = context.comptime().slot(place) else {
        return Ok(());
    };
    let subject = match slot.name() {
        Some(name) => format!("'{name}'"),
        None => "this temporary".into(),
    };
    match slot.state() {
        ComptimeSlotState::Live => Ok(()),
        ComptimeSlotState::Uninitialized => Err(hmir_error(span, &mir::COMPTIME_UNSET, subject)),
        ComptimeSlotState::Invalidated => {
            let (function, place, discarded) = ownership_subject(context, slot);
            let args = (function, place, operation.into(), discarded);
            Err(hmir_error(span, &analysis::AFTER_MOVE, args))
        }
    }
}

fn ownership_subject(
    context: &LocalLoweringContext,
    slot: &ComptimeSlot,
) -> (String, String, bool) {
    let function = context
        .comptime()
        .function()
        .map_or_else(String::new, |def| symbol::name(context.env(), def));
    let place = slot
        .name()
        .map_or_else(|| "temporary".into(), ToString::to_string);
    let discarded = place == "_";
    (function, place, discarded)
}

fn require_droppable(
    context: &LocalLoweringContext,
    slot: &ComptimeSlot,
    span: &TokenRange,
) -> CHLResult<()> {
    if context.env().is_nodrop(slot.ty()) {
        let (function, place, discarded) = ownership_subject(context, slot);
        let args = (
            function,
            "value".into(),
            place,
            "lifetime".into(),
            discarded,
        );
        return Err(hmir_error(span, &analysis::VALUE_NOT_CONSUMED, args));
    }
    Ok(())
}

// The contents of the storage a reference designates in the form they have outside of comptime storage, which
// fails where they hold an address of it
pub fn contents(
    context: &mut LocalLoweringContext,
    reference: &HMIRValue,
    span: &TokenRange,
) -> CHLResult<HMIRValue> {
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
) -> CHLResult<HMIRValue> {
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
) -> CHLResult<usize> {
    if let HMIRValueKind::Immediate(HMIRImmValue::Union { variant, .. }) = value.kind() {
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
) -> CHLResult<()> {
    let location = locate(context, target, true, span)?;
    let ty = context.env().referent(target.ty());
    let bitfield = types::bitfield(context.env(), target.ty());
    match is_held(context, value) {
        true => {
            let source = locate(context, value, false, span)?;
            context.copy(&location, &source, ty, span)?;
        }
        false => context.store(&location, ty, value.kind(), bitfield, span)?,
    }
    Ok(())
}

// Whether 'value' is held in comptime storage rather than an address of it, as an aggregate read out of it is
fn is_held(context: &LocalLoweringContext, value: &HMIRValue) -> bool {
    matches!(value.kind(), HMIRValueKind::Reference(_))
        && !context.env().is_reference(value.ty())
        && !context.env().is_pointer(value.ty())
}

pub fn initialize(
    context: &mut LocalLoweringContext,
    target: HMIRPlaceID,
    span: &TokenRange,
) -> CHLResult<()> {
    let slot = context
        .comptime()
        .slot(target)
        .expect("initialization requires a slot");
    if slot.state() == ComptimeSlotState::Live {
        require_droppable(context, slot, span)?;
        return Err(hmir_error(
            span,
            &analysis::ALREADY_INITIALIZED,
            ownership_subject(context, slot),
        ));
    }
    context
        .comptime_mut()
        .slot_mut(target)
        .expect("initialization requires a slot")
        .set_state(ComptimeSlotState::Live);
    Ok(())
}

pub fn invalidate(
    context: &mut LocalLoweringContext,
    target: HMIRPlaceID,
    kind: MIRInvalidationKind,
    span: &TokenRange,
) -> CHLResult<()> {
    let slot = context
        .comptime()
        .slot(target)
        .expect("invalidation requires a slot");
    let state = match kind {
        MIRInvalidationKind::Drop => {
            if slot.state() != ComptimeSlotState::Live {
                return Ok(());
            }
            require_droppable(context, slot, span)?;
            ComptimeSlotState::Uninitialized
        }
        MIRInvalidationKind::Move | MIRInvalidationKind::Leak => {
            let operation = if kind == MIRInvalidationKind::Move {
                "was moved"
            } else {
                "was leaked"
            };
            require_live(context, target, operation, span)?;
            ComptimeSlotState::Invalidated
        }
    };
    context
        .comptime_mut()
        .slot_mut(target)
        .expect("invalidation requires a slot")
        .set_state(state);
    Ok(())
}
