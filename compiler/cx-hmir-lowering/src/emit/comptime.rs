pub mod float;
pub mod int;
pub mod op;

pub use op::{binary, cast, difference, offset, unary};

use cx_hmir::ty::{HMIRTypeID, aggregate::HMIRBitfield, context::HMIRTypeContext};
use cx_log::{
    CXResult,
    catalogue::{analysis, mir},
};
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::function::{
        LocalLoweringContext,
        comptime::ComptimeSlotState,
        memory::{field_offset, layout},
    },
    eval::types,
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarFault {
    ZeroDivisor,
    Undefined,
}

pub fn null_check(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange
) -> CXResult<()> {
    let immediate : HMIRValueKind = todo!();

    match immediate {
        HMIRValueKind::Immediate(HMIRImmediate::Int(0)) |
        HMIRValueKind::Immediate(HMIRImmediate::Unit) => {
            Err(
                hmir_error(span, &typecheck::COMPTIME_INTERPRETATION, "attempted to dereference a null pointer")
            )
        },

        _ => Ok(())
    }
}

pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let region = context.allocate(ty, span)?;
    context.comptime_mut().declare_slot(region, ty, name);

    Ok(HMIRValue::place(ty, HMIRReference::Region(region)))
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some(mut constant) = HMIRImmediate::zero(context.env(), ty) else {
        unreachable!("an initialized aggregate has a zero value")
    };
    for (index, value) in fields {
        let value = rvalue(context, value, span)?;
        match &mut constant {
            HMIRImmediate::Aggregate(items) => items[*index] = value,
            HMIRImmediate::Union {
                member,
                value: slot,
            }
            | HMIRImmediate::TaggedUnion {
                variant: member,
                payload: slot,
            } => (*member, **slot) = (*index, value),
            _ => unreachable!("an initialized aggregate has an aggregate zero value"),
        }
    }

    Ok(HMIRValue::constant(ty, constant))
}

fn storage(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<Option<(HMIRAddress, HMIRTypeID)>> {
    let inner = context.env().reference_inner(value.ty());
    Ok(match value.kind() {
        HMIRValueKind::Reference(place @ HMIRReference::Region(region)) => {
            let slot = context.comptime().slot(*region).map(|slot| slot.ty());
            Some((HMIRAddress::place(*place), slot.unwrap_or(value.ty())))
        }
        HMIRValueKind::Reference(place) => {
            Some((HMIRAddress::place(*place), inner.unwrap_or(value.ty())))
        }
        HMIRValueKind::Immediate(HMIRImmediate::Address(address)) => {
            inner.map(|inner| (address.clone(), inner))
        }
        HMIRValueKind::Indirect(pointer) => match rvalue(context, pointer, span)? {
            HMIRImmediate::Address(address) => Some((address, value.ty())),
            _ => None,
        },
        HMIRValueKind::Immediate(_) => None,
    })
}

fn tracked(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<Option<HMIRRegionID>> {
    let Some((address, ty)) = storage(context, value, span)? else {
        return Ok(None);
    };
    let Some(HMIRReference::Region(region)) = address.whole_place() else {
        return Ok(None);
    };
    let Some(slot) = context.comptime().slot(region).map(|slot| slot.ty()) else {
        return Ok(None);
    };
    let whole = layout(context.env_mut(), ty, span)? == layout(context.env_mut(), slot, span)?;

    Ok(whole.then_some(region))
}

fn live(
    context: &LocalLoweringContext,
    address: &HMIRAddress,
    writing: bool,
    span: &TokenRange,
) -> CXResult<()> {
    let Addressable::Place(HMIRReference::Region(region)) = address.base() else {
        return Ok(());
    };
    let Some(slot) = context.comptime().slot(*region) else {
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

fn read(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRImmediate> {
    let Some((address, ty)) = storage(context, value, span)? else {
        let HMIRValueKind::Immediate(constant) = value.kind() else {
            unreachable!("a value without storage is a constant")
        };
        return Ok(constant.clone());
    };
    live(context, &address, false, span)?;
    let bitfield = types::bitfield(context.env(), value);
    context.load(&address, ty, bitfield, span)
}

fn rvalue(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRImmediate> {
    match storage(context, value, span)? {
        Some((address, stored))
            if context.env().is_reference(value.ty()) && !context.env().is_reference(stored) =>
        {
            Ok(HMIRImmediate::Address(address))
        }
        _ => read(context, value, span),
    }
}

pub fn copy(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        unreachable!("a comptime copy reads from a reference")
    };

    Ok(HMIRValue::constant(inner, read(context, &value, span)?))
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    let Some((address, ty)) = storage(context, target, span)? else {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_INVALID_OPERATION,
            "writing through this address".into(),
        ));
    };
    if tracked(context, target, span)?.is_none() {
        live(context, &address, true, span)?;
    }
    let bitfield = types::bitfield(context.env(), target);
    let packed = bitfield.is_some() || types::bitfield(context.env(), value).is_some();
    match storage(context, value, span)? {
        Some((source, stored))
            if !packed
                && (!context.env().is_reference(ty) || context.env().is_reference(stored)) =>
        {
            live(context, &source, false, span)?;
            context.copy(&address, &source, ty, span)?;
        }
        _ => {
            let constant = rvalue(context, value, span)?;
            context.store(&address, ty, &constant, bitfield, span)?;
        }
    }
    if let Addressable::Place(HMIRReference::Region(region)) = address.base()
        && let Some(slot) = context.comptime_mut().slot_mut(*region)
    {
        slot.set_state(ComptimeSlotState::Live);
    }

    Ok(())
}

fn mark(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    state: ComptimeSlotState,
    span: &TokenRange,
) {
    if let Ok(Some(region)) = tracked(context, value, span)
        && let Some(slot) = context.comptime_mut().slot_mut(region)
    {
        slot.set_state(state);
    }
}

pub fn initialize(context: &mut LocalLoweringContext, value: &HMIRValue, span: &TokenRange) {
    mark(context, value, ComptimeSlotState::Live, span);
}

pub fn invalidate(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    kind: MIRInvalidationKind,
    span: &TokenRange,
) {
    let state = match kind {
        MIRInvalidationKind::Drop => ComptimeSlotState::Uninitialized,
        MIRInvalidationKind::Move | MIRInvalidationKind::Leak => ComptimeSlotState::Invalidated,
    };
    mark(context, value, state, span);
}

pub fn tag(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let HMIRImmediate::TaggedUnion { variant, .. } = read(context, value, span)? else {
        unreachable!("a tagged union constant names its variant")
    };

    Ok(HMIRValue::constant(ty, HMIRImmediate::Int(variant as i128)))
}

pub fn address(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some((address, _)) = storage(context, value, span)? else {
        unreachable!("only storage has an address")
    };

    Ok(HMIRValue::constant(ty, HMIRImmediate::Address(address)))
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let HMIRImmediate::Address(address) = rvalue(context, &pointer, span)? else {
        let operation = "dereference".into();
        return Err(hmir_error(
            span,
            &mir::COMPTIME_UNDEFINED_ARITHMETIC,
            operation,
        ));
    };

    Ok(HMIRValue::indirect(
        inner,
        HMIRValue::constant(pointer.ty(), HMIRImmediate::Address(address)),
    ))
}

pub fn field(
    context: &mut LocalLoweringContext,
    base: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    bitfield: Option<HMIRBitfield>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (address, owner) = match (storage(context, base, span)?, base.kind()) {
        (Some(storage), _) => storage,
        (None, HMIRValueKind::Immediate(HMIRImmediate::Aggregate(items))) => {
            return Ok(HMIRValue::constant(ty, items[index].clone()));
        }
        (
            None,
            HMIRValueKind::Immediate(
                HMIRImmediate::Union { member, value }
                | HMIRImmediate::TaggedUnion {
                    variant: member,
                    payload: value,
                },
            ),
        ) if *member == index => {
            return Ok(HMIRValue::constant(ty, value.as_ref().clone()));
        }
        (None, _) => {
            let temporary = allocate(context, base.ty(), None, span)?;
            write(context, &temporary, base, span)?;
            (
                HMIRAddress::place(match temporary.kind() {
                    HMIRValueKind::Reference(place) => *place,
                    _ => unreachable!("comptime storage is a place"),
                }),
                base.ty(),
            )
        }
    };
    let offset = field_offset(context.env_mut(), owner, index, span)?;
    let Some(address) = i64::try_from(offset)
        .ok()
        .and_then(|offset| address.checked_add(offset))
    else {
        return Err(hmir_error(span, &mir::COMPTIME_POINTER_OVERFLOW, ()));
    };
    let reference = types::reference(context.env_mut(), ty, bitfield, span);
    let pointer = HMIRValue::constant(reference, HMIRImmediate::Address(address));

    // A bitfield only covers part of its storage unit, so it never names a whole place
    Ok(match bitfield {
        Some(_) => HMIRValue::new(ty, HMIRValueKind::Indirect(Box::new(pointer))),
        None => HMIRValue::indirect(ty, pointer),
    })
}
