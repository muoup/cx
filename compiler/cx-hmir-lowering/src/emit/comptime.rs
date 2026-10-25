mod op;

pub use op::{binary, cast, difference, offset, unary};

use cx_hmir::ty::{HMIRTypeID, context::HMIRTypeContext};
use cx_log::{CXResult, catalogue::mir};
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
    value::{
        HMIRPlace, HMIRRegionID, HMIRValue, HMIRValueKind,
        address::{Addressable, HMIRAddress},
        constant::HMIRConstant,
    },
};

pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let region = context.allocate(ty, span)?;
    context.comptime_mut().declare_slot(region, ty, name);

    Ok(HMIRValue::place(ty, HMIRPlace::Region(region)))
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some(mut constant) = HMIRConstant::zero(context.env(), ty) else {
        unreachable!("an initialized aggregate has a zero value")
    };
    for (index, value) in fields {
        let value = rvalue(context, value, span)?;
        match &mut constant {
            HMIRConstant::Aggregate(items) => items[*index] = value,
            HMIRConstant::Union {
                member,
                value: slot,
            }
            | HMIRConstant::TaggedUnion {
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
        HMIRValueKind::PlaceRef(place @ HMIRPlace::Region(region)) => {
            let slot = context.comptime().slot(*region).map(|slot| slot.ty());
            Some((HMIRAddress::place(*place), slot.unwrap_or(value.ty())))
        }
        HMIRValueKind::PlaceRef(place) => {
            Some((HMIRAddress::place(*place), inner.unwrap_or(value.ty())))
        }
        HMIRValueKind::Constant(HMIRConstant::Address(address)) => {
            inner.map(|inner| (address.clone(), inner))
        }
        HMIRValueKind::Indirect(pointer) => match rvalue(context, pointer, span)? {
            HMIRConstant::Address(address) => Some((address, value.ty())),
            _ => None,
        },
        HMIRValueKind::Constant(_) => None,
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
    let Some(HMIRPlace::Region(region)) = address.whole_place() else {
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
    let Addressable::Place(HMIRPlace::Region(region)) = address.base() else {
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
        ComptimeSlotState::Invalidated => Err(hmir_error(
            span,
            &mir::COMPTIME_UNSET,
            format!("{subject}, which was moved or leaked,"),
        )),
    }
}

fn read(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRConstant> {
    let Some((address, ty)) = storage(context, value, span)? else {
        let HMIRValueKind::Constant(constant) = value.kind() else {
            unreachable!("a value without storage is a constant")
        };
        return Ok(constant.clone());
    };
    live(context, &address, false, span)?;
    context.load(&address, ty, span)
}

fn rvalue(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRConstant> {
    match storage(context, value, span)? {
        Some((address, stored))
            if context.env().is_reference(value.ty()) && !context.env().is_reference(stored) =>
        {
            Ok(HMIRConstant::Address(address))
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
    match storage(context, value, span)? {
        Some((source, stored))
            if !context.env().is_reference(ty) || context.env().is_reference(stored) =>
        {
            live(context, &source, false, span)?;
            context.copy(&address, &source, ty, span)?;
        }
        _ => {
            let constant = rvalue(context, value, span)?;
            context.store(&address, ty, &constant, span)?;
        }
    }
    if let Addressable::Place(HMIRPlace::Region(region)) = address.base()
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
    let HMIRConstant::TaggedUnion { variant, .. } = read(context, value, span)? else {
        unreachable!("a tagged union constant names its variant")
    };

    Ok(HMIRValue::constant(ty, HMIRConstant::Int(variant as i128)))
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

    Ok(HMIRValue::constant(ty, HMIRConstant::Address(address)))
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let HMIRConstant::Address(address) = rvalue(context, &pointer, span)? else {
        let operation = "dereference".into();
        return Err(hmir_error(
            span,
            &mir::COMPTIME_UNDEFINED_ARITHMETIC,
            operation,
        ));
    };

    Ok(HMIRValue::indirect(
        inner,
        HMIRValue::constant(pointer.ty(), HMIRConstant::Address(address)),
    ))
}

pub fn field(
    context: &mut LocalLoweringContext,
    base: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (address, owner) = match (storage(context, base, span)?, base.kind()) {
        (Some(storage), _) => storage,
        (None, HMIRValueKind::Constant(HMIRConstant::Aggregate(items))) => {
            return Ok(HMIRValue::constant(ty, items[index].clone()));
        }
        (
            None,
            HMIRValueKind::Constant(
                HMIRConstant::Union { member, value }
                | HMIRConstant::TaggedUnion {
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
                    HMIRValueKind::PlaceRef(place) => *place,
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
    let reference = types::reference(context.env_mut(), ty, span);

    Ok(HMIRValue::indirect(
        ty,
        HMIRValue::constant(reference, HMIRConstant::Address(address)),
    ))
}
