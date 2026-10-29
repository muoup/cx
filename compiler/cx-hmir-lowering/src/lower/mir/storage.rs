use cx_hmir::ty::{
    HMIRTypeID, HMIRTypeKind, aggregate::HMIRBitfield, context::HMIRTypeContext,
    intrinsic::HMIRIntWidth,
};
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateIntrinsic, MIRBindable, MIRBitfieldAccess, MIRConstant, MIRInstructionKind,
    MIRInternalIntrinsic, MIRPlaceDecl, MIRPtrIntrinsic, MIRScopeID, MIRStoreBitfield, MIRTarget,
    MIRTypeID, MIRValue, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use super::{initialized, intrinsic, operand, output, register, value::reference};
use crate::{
    env::function::{LocalLoweringContext, runtime::Slot},
    eval::types,
    lower::{global, types::lower_type},
    value::{
        HMIRPlace, HMIRValue, HMIRValueKind,
        address::{Addressable, HMIRAddress},
        constant::HMIRConstant,
    },
};

fn allocate_place(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    scope: MIRScopeID,
    nodrop: bool,
    adopted: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let mir_ty = lower_type(context.env_mut(), ty, span)?;
    let place = context
        .runtime_mut()
        .builder_mut()
        .allocate_place(MIRPlaceDecl::new(mir_ty, name, scope, nodrop, adopted));

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

    allocate_place(context, ty, name, scope, nodrop, false, span)
}

// The number of places declared so far that a scope drops, as opposed to scratch storage
pub fn scoped_places(context: &LocalLoweringContext) -> usize {
    let runtime = context.runtime();
    runtime.builder().places().iter().filter(|place| place.scope() != runtime.scratch_scope()).count()
}

pub fn scratch(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let scope = context.runtime().scratch_scope();

    allocate_place(context, ty, None, scope, false, false, span)
}

// Storage of type 'ty' declared in the current scope over the memory 'source' references, which it takes the
// obligations of
pub fn adopt(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    source: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let address = operand(context, source, span)?;
    let nodrop = context.env().is_nodrop(ty);
    let scope = context.runtime().current_scope();
    let target = allocate_place(context, ty, name, scope, nodrop, true, span)?;
    let Some(MIRBindable::Place(place)) = bindable(context, &target) else {
        unreachable!("an adopting place is declared")
    };
    intrinsic(context, MIRInternalIntrinsic::AdoptPlace { place, address }, span);
    initialize(context, &target, span);

    Ok(target)
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
        Some(_) => match operand(context, value, span)? {
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
    let runtime_value = operand(context, &value, span)?;
    let Some(result_type) = context.env().reference_inner(value.ty()) else {
        unreachable!("read from type: {}", types::name(context.env(), value.ty()))
    };
    let bitfield = types::bitfield(context.env(), &value)
        .map(|bitfield| MIRStoreBitfield::Source(access(context, result_type, bitfield)));

    let ty = lower_type(context.env_mut(), result_type, span)?;
    let (target, result) = output(context, result_type, span)?;
    store(context, target, runtime_value, ty, bitfield, span);

    Ok(initialized(context, result, span))
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
    let stored = context.env().referent(target.ty());
    let bitfield = types::bitfield(context.env(), target)
        .map(|bitfield| MIRStoreBitfield::Target(access(context, stored, bitfield)));
    let ty = lower_type(context.env_mut(), stored, span)?;
    let source = operand(context, value, span)?;
    store(context, destination, source, ty, bitfield, span);

    Ok(())
}

fn store(
    context: &mut LocalLoweringContext,
    target: MIRTarget,
    value: MIRValue,
    ty: MIRTypeID,
    bitfield: Option<MIRStoreBitfield>,
    span: &TokenRange,
) {
    let store = MIRInstructionKind::Store { target, value, ty, bitfield };
    context.runtime_mut().emit(store, span.clone())
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

pub fn address(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let place = |place| Ok(HMIRValue::constant(ty, HMIRConstant::Address(HMIRAddress::place(place))));
    let reference = match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Address(address)) => {
            return Ok(HMIRValue::constant(ty, HMIRConstant::Address(address.clone())));
        }
        HMIRValueKind::PlaceRef(local @ HMIRPlace::Local(slot)) => {
            match context.runtime().slot(*slot).binding() {
                MIRBindable::Place(_) => return place(*local),
                MIRBindable::Register(_) if context.env().is_reference(value.ty()) => value,
                MIRBindable::Register(_) => unreachable!("a register has no address"),
            }
        }
        HMIRValueKind::PlaceRef(other) => return place(*other),
        HMIRValueKind::Indirect(pointer) => pointer,
        HMIRValueKind::Constant(_) => unreachable!("only storage has an address"),
    };

    let reference = operand(context, reference, span)?;
    let (out, result) = output(context, ty, span)?;
    intrinsic(context, MIRInternalIntrinsic::ReferenceAddress { out, reference }, span);

    Ok(initialized(context, result, span))
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
    base: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = operand(context, &pointer, span)?;
    let base = base.unwrap_or(&pointer);
    let checked = operand(context, base, span)?;

    let null = MIRConstant::Nullptr {
        ty: lower_type(context.env_mut(), base.ty(), span)?,
    };
    let bool_ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let condition = register(context, bool_ty, span)?;
    let check = MIRPtrIntrinsic::Neq {
        out: MIRTarget::Register(condition),
        lhs: checked,
        rhs: MIRValue::Constant(null),
    };
    intrinsic(context, check, span);
    let message = Some("dereferenced a null pointer".into());
    intrinsic(context, MIRInternalIntrinsic::Assert { condition: MIRValue::Register(condition), message }, span);

    let reference_ty = types::reference(context.env_mut(), inner, None, span);
    let target_ty = lower_type(context.env_mut(), reference_ty, span)?;
    let (out, reference) = output(context, reference_ty, span)?;
    intrinsic(context, MIRInternalIntrinsic::Bitcast { out, value, target_ty }, span);

    Ok(HMIRValue::indirect(inner, initialized(context, reference, span)))
}

pub fn field(
    context: &mut LocalLoweringContext,
    base: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    bitfield: Option<HMIRBitfield>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let stored = context.env().referent(base.ty());
    let tagged = matches!(context.env().resolve_kind(stored), HMIRTypeKind::TaggedUnion { .. });
    let aggregate_ty = lower_type(context.env_mut(), stored, span)?;
    let base = operand(context, base, span)?;

    let reference_ty = types::reference(context.env_mut(), ty, bitfield, span);
    let (out, reference) = output(context, reference_ty, span)?;
    let projection = match tagged {
        true => MIRAggregateIntrinsic::SumVariant { out, base, variant: index, sum_ty: aggregate_ty },
        false => MIRAggregateIntrinsic::StructField { out, base, field: index, struct_ty: aggregate_ty },
    };
    intrinsic(context, projection, span);

    Ok(HMIRValue::indirect(ty, initialized(context, reference, span)))
}
