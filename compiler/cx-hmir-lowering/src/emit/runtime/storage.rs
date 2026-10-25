use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, aggregate::HMIRBitfield, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_mir::{
    MIRBindable, MIRBitfieldAccess, MIRInstructionKind, MIRInternalIntrinsic, MIRPlaceDecl,
    MIRScopeID, MIRStoreBitfield, MIRTarget, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use super::{global, intrinsic, lower_value, output, reference, register, store};
use crate::{
    env::function::{LocalLoweringContext, runtime::Slot},
    eval::types,
    value::{HMIRPlace, HMIRValue, HMIRValueKind, address::Addressable, constant::HMIRConstant},
};

fn allocate_place(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    scope: MIRScopeID,
    nodrop: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let mir_ty = types::lower_type(context.env_mut(), ty, span)?;
    let place = context
        .runtime_mut()
        .builder_mut()
        .allocate_place(MIRPlaceDecl::new(mir_ty, name, scope, nodrop, false));

    Ok(slot(context, ty, MIRBindable::Place(place)))
}

// Registers a MIR binding as a fresh evaluation slot; this names storage and never binds a local
pub fn slot(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    bindable: MIRBindable,
) -> HMIRValue {
    let slot = context.runtime_mut().allocate_slot(Slot::new(bindable, None));

    HMIRValue::place(ty, HMIRPlace::Local(slot))
}

fn bindable(context: &LocalLoweringContext, value: &HMIRValue) -> Option<MIRBindable> {
    let is_reference = context.env().is_reference(value.ty());
    let place = match value.kind() {
        HMIRValueKind::PlaceRef(place) => *place,
        HMIRValueKind::Constant(HMIRConstant::Address(address)) if is_reference => {
            match address.whole_place() {
                Some(place) => place,
                None => return None,
            }
        }
        _ => return None,
    };
    let HMIRPlace::Local(slot) = place else {
        return None;
    };

    match context.runtime().slot(slot).binding() {
        MIRBindable::Register(_) if is_reference => None,
        bindable => Some(bindable.clone()),
    }
}

// Where the storage of a value can be written: for a reference, the storage it designates, otherwise the value's
// own place or the storage behind its pointer; a register has no address
fn target(context: &mut LocalLoweringContext, value: &HMIRValue, span: &TokenRange) -> CXResult<Option<MIRTarget>> {
    let is_reference = context.env().is_reference(value.ty());
    Ok(match value.kind() {
        HMIRValueKind::PlaceRef(HMIRPlace::Global(def)) => Some(MIRTarget::Global(
            global::address(context.env_mut(), *def, 0, value.ty(), span)?,
        )),
        HMIRValueKind::Constant(HMIRConstant::Address(address))
            if is_reference && let Addressable::Place(HMIRPlace::Global(def)) = address.base() =>
        {
            let inner = context.env().reference_inner(value.ty()).unwrap();
            let global = global::address(context.env_mut(), *def, address.offset(), inner, span)?;
            Some(MIRTarget::Global(global))
        }
        HMIRValueKind::Indirect(pointer) if !is_reference => {
            Some(MIRTarget::Indirect(reference(context, pointer)))
        }
        HMIRValueKind::Indirect(_) => todo!("write to a stored reference"),
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) if is_reference => {
            match context.runtime().slot(*slot).binding() {
                MIRBindable::Register(register) => Some(MIRTarget::Indirect(*register)),
                MIRBindable::Place(place) => Some(MIRTarget::Place(*place)),
            }
        }
        _ => match bindable(context, value) {
            Some(MIRBindable::Place(place)) => Some(MIRTarget::Place(place)),
            _ => None,
        },
    })
}

pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let nodrop = context.env().is_nodrop(ty);
    let scope = context.runtime().current_scope();

    allocate_place(context, ty, name, scope, nodrop, span)
}

pub fn scratch(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let scope = context.runtime().scratch_scope();

    allocate_place(context, ty, None, scope, false, span)
}

pub fn scratch_held(context: &LocalLoweringContext, value: &HMIRValue) -> bool {
    let HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) = value.kind() else {
        return false;
    };
    let runtime = context.runtime();
    match runtime.slot(*slot).binding() {
        MIRBindable::Place(place) if !context.env().is_reference(value.ty()) => runtime
            .builder()
            .place(*place)
            .is_some_and(|place| place.scope() == runtime.scratch_scope()),
        _ => false,
    }
}

pub fn place_held(context: &LocalLoweringContext, value: &HMIRValue) -> bool {
    let HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) = value.kind() else {
        return false;
    };
    matches!(context.runtime().slot(*slot).binding(), MIRBindable::Place(_))
}

pub fn adopt(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    source: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    let Some(MIRBindable::Place(place)) = bindable(context, target) else {
        unreachable!("only a place adopts storage")
    };
    let address = lower_value(context, source, span)?;
    let builder = context.runtime_mut().builder_mut();
    let decl = builder.place_mut(place).expect("an adopting place is declared");
    *decl = MIRPlaceDecl::new(decl.ty(), decl.debug_name().cloned(), decl.scope(), decl.nodrop(), true);
    intrinsic(context, MIRInternalIntrinsic::AdoptPlace { place, address }, span.clone());
    initialize(context, target, span);

    Ok(())
}

pub fn parameter(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = allocate(context, ty, name, span)?;
    let Some(MIRBindable::Place(place)) = bindable(context, &value) else {
        unreachable!("a parameter is allocated as a place")
    };
    context.runtime_mut().builder_mut().add_parameter(place);
    initialize(context, &value, span);

    Ok(value)
}

pub fn initialize(context: &mut LocalLoweringContext, value: &HMIRValue, span: &TokenRange) {
    if let Some(place @ MIRBindable::Place(_)) = bindable(context, value) {
        context
            .runtime_mut()
            .emit(MIRInstructionKind::Initialize { place }, span.clone());
    }
}

pub fn invalidate(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    kind: MIRInvalidationKind,
    span: &TokenRange,
) {
    if let Some(place @ MIRBindable::Place(_)) = bindable(context, value) {
        context
            .runtime_mut()
            .emit(MIRInstructionKind::Invalidate { place, kind }, span.clone());
    }
}

pub fn lift(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    origin: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source = match origin {
        Some(_) => match lower_value(context, value, span)? {
            MIRValue::Register(register) => MIRBindable::Register(register),
            _ => unreachable!("a projected reference is held in a register"),
        },
        None => bindable(context, value).expect("a lift reads a tracked place"),
    };
    let origin = bindable(context, origin.unwrap_or(value)).expect("a lift has a tracked origin");
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        unreachable!("a lift reads through a reference")
    };
    let out = register(context, inner, span)?;
    context.runtime_mut().emit(
        MIRInstructionKind::Lift {
            out,
            source,
            origin,
        },
        span.clone(),
    );
    Ok(slot(context, inner, MIRBindable::Register(out)))
}

pub fn copy(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let runtime_value = lower_value(context, &value, span)?;
    let Some(result_type) = context.env().reference_inner(value.ty()) else {
        unreachable!("read from type: {}", types::name(context.env(), value.ty()))
    };
    let bitfield = types::bitfield(context.env(), &value)
        .map(|bitfield| MIRStoreBitfield::Source(access(context, result_type, bitfield)));

    let ty = types::lower_type(context.env_mut(), result_type, span)?;
    let (out, result) = output(context, result_type, span)?;
    context.runtime_mut().emit(
        MIRInstructionKind::Store {
            target: out,
            value: runtime_value,
            ty,
            bitfield,
        },
        span.clone(),
    );
    initialize(context, &result, span);

    Ok(result)
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    let Some(destination) = self::target(context, target, span)? else {
        unreachable!("write to a value without an address");
    };
    let stored = context
        .env()
        .reference_inner(target.ty())
        .unwrap_or(target.ty());
    let bitfield = types::bitfield(context.env(), target)
        .map(|bitfield| MIRStoreBitfield::Target(access(context, stored, bitfield)));
    let ty = types::lower_type(context.env_mut(), stored, span)?;
    let source = lower_value(context, value, span)?;
    store(context, destination, source, ty, bitfield, span.clone());

    Ok(())
}

fn access(context: &LocalLoweringContext, ty: HMIRTypeID, bitfield: HMIRBitfield) -> MIRBitfieldAccess {
    let HMIRTypeKind::Int(int) = context.env().resolve_kind(ty) else {
        unreachable!("a bitfield holds an integer")
    };
    MIRBitfieldAccess {
        bit_offset: bitfield.offset,
        bit_width: bitfield.width,
        signed: int.signed(),
    }
}
