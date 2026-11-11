use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, aggregate::HMIRBitfield, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_mir::{
    MIRBindable, MIRBitfieldAccess, MIRInstructionKind, MIRInternalIntrinsic,
    MIRPlaceID, MIRStoreBitfield, MIRTarget, MIRTypeID, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit::runtime,
    env::function::LocalLoweringContext,
    eval::types,
    lower::{global, types::lower_type},
    value::{
        HMIRPlaceID, HMIRValue, HMIRValueKind,
        address::{Addressable, HMIRAddress},
        immediate::HMIRImmediate,
    },
};

// The storage a reference designates
fn target(
    context: &mut LocalLoweringContext,
    reference: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRTarget> {
    let stored = context.env().referent(reference.ty());
    Ok(match reference.kind() {
        HMIRValueKind::Reference(address) if address.offset() == 0 => {
            MIRTarget::Place(context.runtime().place(*address.base()))
        }
        HMIRValueKind::Reference(_) => MIRTarget::Indirect {
            value: MIRValue::Constant(runtime::local(context, reference)),
            ty: lower_type(context.env_mut(), stored, span)?,
        },
        HMIRValueKind::Immediate(HMIRImmediate::Address(address))
            if let Addressable::Def(def) = address.base() =>
        {
            let global = global::reference(context.env_mut(), *def, address.offset(), stored, span)?;
            MIRTarget::Global(global)
        }
        _ => MIRTarget::Indirect {
            value: runtime::value(context, reference, span)?,
            ty: lower_type(context.env_mut(), stored, span)?,
        },
    })
}

// The contents of the storage a reference designates as an operand, read where the operand is used
pub(crate) fn contents(
    context: &mut LocalLoweringContext,
    reference: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    let stored = context.env().referent(reference.ty());
    let ty = lower_type(context.env_mut(), stored, span)?;
    Ok(match target(context, reference, span)? {
        MIRTarget::Place(place) => MIRValue::PlaceRef { place, ty },
        MIRTarget::Global(global) => MIRValue::GlobalRef(global),
        MIRTarget::Indirect { value, .. } => MIRValue::Indirect {
            value: Box::new(value),
            ty,
        },
        MIRTarget::Register(_) => unreachable!("a register has no address"),
    })
}

fn declared(
    context: &mut LocalLoweringContext,
    place: HMIRPlaceID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> HMIRValue {
    let reference = types::reference(context.env_mut(), ty, None, span);
    HMIRValue::new(reference, HMIRAddress::new(place, 0))
}

fn place(context: &LocalLoweringContext, reference: &HMIRValue) -> Option<MIRPlaceID> {
    match reference.kind() {
        HMIRValueKind::Reference(address) if address.offset() == 0 => {
            Some(context.runtime().place(*address.base()))
        }
        _ => None,
    }
}

pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let nodrop = context.env().is_nodrop(ty);
    let place = runtime::place(context, ty, name, nodrop, false, span)?;
    context.runtime_mut().declare(place);

    Ok(declared(context, place, ty, span))
}

pub fn scratch(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let place = runtime::place(context, ty, None, false, false, span)?;

    Ok(declared(context, place, ty, span))
}

pub fn adopt(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    source: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let address = runtime::value(context, source, span)?;
    let nodrop = context.env().is_nodrop(ty);
    let place = runtime::place(context, ty, name, nodrop, true, span)?;
    context.runtime_mut().declare(place);

    let adopt = MIRInternalIntrinsic::AdoptPlace {
        place: context.runtime().place(place),
        address,
    };
    context.runtime_mut().emit_intrinsic(adopt, span.clone());
    let target = declared(context, place, ty, span);
    initialize(context, &target, span);

    Ok(target)
}

pub fn parameter(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let target = allocate(context, ty, name, span)?;
    let Some(place) = place(context, &target) else {
        unreachable!("a parameter is allocated as a place")
    };
    context.runtime_mut().builder_mut().add_parameter(place);
    initialize(context, &target, span);

    Ok(target)
}

pub fn initialize(context: &mut LocalLoweringContext, target: &HMIRValue, span: &TokenRange) {
    if let Some(place) = place(context, target) {
        context
            .runtime_mut()
            .emit(MIRInstructionKind::Initialize { place }, span.clone());
    }
}

pub fn invalidate(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    kind: MIRInvalidationKind,
    span: &TokenRange,
) {
    if let Some(place) = place(context, target) {
        context
            .runtime_mut()
            .emit(MIRInstructionKind::Invalidate { place, kind }, span.clone());
    }
}

pub fn lift(
    context: &mut LocalLoweringContext,
    source: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let origin = match source.kind() {
        HMIRValueKind::Reference(address) => {
            MIRBindable::Place(context.runtime().place(*address.base()))
        }
        HMIRValueKind::Register(register) => {
            MIRBindable::Register(context.runtime().register(*register))
        }
        HMIRValueKind::Immediate(_) => unreachable!("a lift reads out of local storage"),
    };
    let inner = context.env().referent(source.ty());
    let source = target(context, source, span)?;
    let (out, result) = runtime::output(context, inner, span)?;
    let MIRTarget::Register(out) = out else {
        unreachable!("an output is a register")
    };

    let lift = MIRInstructionKind::Lift { out, source, origin };
    context.runtime_mut().emit(lift, span.clone());

    Ok(result)
}

// The contents of the storage a reference designates, copied out as they are now
pub fn read(
    context: &mut LocalLoweringContext,
    source: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let inner = context.env().referent(source.ty());
    let bitfield = types::bitfield(context.env(), source.ty())
        .map(|bitfield| MIRStoreBitfield::Source(access(context, inner, bitfield)));
    let value = contents(context, source, span)?;
    let ty = lower_type(context.env_mut(), inner, span)?;
    let (target, result) = runtime::output(context, inner, span)?;
    store(context, target, value, ty, bitfield, span);

    Ok(result)
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    let bitfield = types::bitfield(context.env(), target.ty())
        .map(|bitfield| MIRStoreBitfield::Target(access(context, value.ty(), bitfield)));
    let destination = self::target(context, target, span)?;
    let ty = lower_type(context.env_mut(), value.ty(), span)?;
    let source = runtime::value(context, value, span)?;
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
    let store = MIRInstructionKind::Store {
        target,
        value,
        ty,
        bitfield,
    };
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
