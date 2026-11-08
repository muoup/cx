use cx_hmir::ty::{
    HMIRTypeID, HMIRTypeKind, aggregate::HMIRBitfield, context::HMIRTypeContext,
    intrinsic::HMIRIntWidth,
};
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateIntrinsic, MIRBindable, MIRBitfieldAccess, MIRConstant, MIRInstructionKind,
    MIRInternalIntrinsic, MIRPlaceDecl, MIRPlaceID, MIRPtrIntrinsic, MIRStoreBitfield, MIRTarget,
    MIRTypeID, MIRValue, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use super::register;
use crate::{
    emit::runtime, env::function::LocalLoweringContext, eval::types, lower::{global, types::lower_type}, value::{HMIRRegisterID, HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
};

// Where the storage of a value can be written: for a reference, the storage it designates, otherwise the value's
// own place or the storage behind its pointer; a register has no address
fn target(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRTarget> {
    if let Some(place) = place(context, value) {
        return Ok(MIRTarget::Place(place));
    }
    let is_reference = context.env().is_reference(value.ty());
    let ty = context.env().referent(value.ty());
    Ok(match value.kind() {
        HMIRValueKind::Reference(HMIRReference::Global(def)) => {
            MIRTarget::Global(global::reference(context.env_mut(), *def, 0, ty, span)?)
        }
        HMIRValueKind::Immediate(HMIRImmediate::Address(address))
            if is_reference
                && let Addressable::Place(HMIRReference::Global(def)) = address.base() =>
        {
            let global = global::reference(context.env_mut(), *def, address.offset(), ty, span)?;
            MIRTarget::Global(global)
        }
        _ if is_reference => MIRTarget::Indirect {
            value: super::value(context, value, span)?,
            ty: lower_type(context.env_mut(), ty, span)?,
        },
        HMIRValueKind::Indirect(pointer) => MIRTarget::Indirect {
            value: super::value(context, pointer, span)?,
            ty: lower_type(context.env_mut(), ty, span)?,
        },

        _ => unreachable!("a storage target has an address, found: {:?}", value),
    })
}

pub(crate) fn read(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    let inner = context
        .env()
        .reference_inner(value.ty())
        .expect("a load reads through a reference");
    let ty = lower_type(context.env_mut(), inner, span)?;
    
    Ok(match target(context, value, span)? {
        MIRTarget::Place(place) => MIRValue::PlaceRef { place, ty },
        MIRTarget::Global(global) => MIRValue::GlobalRef(global),
        MIRTarget::Indirect { value, .. } => MIRValue::Indirect {
            value: Box::new(value),
            ty,
        },
        MIRTarget::Register(_) => unreachable!("a register has no address"),
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
    runtime
        .builder()
        .places()
        .iter()
        .filter(|place| place.scope() != runtime.scratch_scope())
        .count()
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
    let address = super::value(context, source, span)?;
    let nodrop = context.env().is_nodrop(ty);
    let scope = context.runtime().current_scope();
    let target = allocate_place(context, ty, name, scope, nodrop, true, span)?;
    let Some(place) = place(context, &target) else {
        unreachable!("an adopting place is declared")
    };

    context
        .runtime_mut()
        .emit_intrinsic(MIRInternalIntrinsic::AdoptPlace { place, address }, span.clone());
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
    let Some(place) = place(context, &value) else {
        unreachable!("a parameter is allocated as a place")
    };
    context.runtime_mut().builder_mut().add_parameter(place);
    initialize(context, &value, span);

    Ok(value)
}

pub fn initialize(context: &mut LocalLoweringContext, value: &HMIRValue, span: &TokenRange) {
    if let Some(place) = place(context, value) {
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
    if let Some(place) = place(context, value) {
        context
            .runtime_mut()
            .emit(MIRInstructionKind::Invalidate { place, kind }, span.clone());
    }
}

pub fn lift(
    context: &mut LocalLoweringContext,
    source: &HMIRValue,
    origin: HMIRRegisterID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some(inner) = context.env().reference_inner(source.ty()) else {
        unreachable!("a lift reads through a reference")
    };

    let origin = context.runtime().register(origin);
    let source = target(context, source, span)?;

    let out = runtime::register(context, inner, span)?;
    let mir_out = context.runtime().register(out);

    context.runtime_mut().emit(
        MIRInstructionKind::Lift {
            out: mir_out,
            source,
            origin: MIRBindable::Register(origin),
        },
        span.clone(),
    );

    Ok(HMIRValue::new(inner, out))
}

pub fn materialize(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = lower_type(context.env_mut(), value.ty(), span)?;
    let source = super::value(context, value, span)?;
    let (target, result) = output(context, value.ty(), span)?;
    store(context, target, source, ty, None, span);

    Ok(result)
}

pub fn capture(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some(place) = place(context, &value) else {
        unreachable!("a storage result is held in a place")
    };
    let ty = lower_type(context.env_mut(), value.ty(), span)?;
    let (target, result) = output(context, value.ty(), span)?;
    store(
        context,
        target,
        MIRValue::PlaceRef { place, ty },
        ty,
        None,
        span,
    );
    Ok(result)
}

pub fn load(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let result_type = context
        .env()
        .reference_inner(value.ty())
        .expect("a load reads through a reference");
    let runtime_value = read(context, &value, span)?;
    let bitfield = types::bitfield(context.env(), &value)
        .map(|bitfield| MIRStoreBitfield::Source(access(context, result_type, bitfield)));

    let ty = lower_type(context.env_mut(), result_type, span)?;
    let (target, result) = output(context, result_type, span)?;
    store(context, target, runtime_value, ty, bitfield, span);

    Ok(result)
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    let destination = self::target(context, target, span)?;
    let stored = value.ty();
    let bitfield = types::bitfield(context.env(), target)
        .map(|bitfield| MIRStoreBitfield::Target(access(context, stored, bitfield)));
    let ty = lower_type(context.env_mut(), stored, span)?;
    let source = super::value(context, value, span)?;
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

fn access(
    context: &LocalLoweringContext,
    ty: HMIRTypeID,
    bitfield: HMIRBitfield,
) -> MIRBitfieldAccess {
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
    let place = |place| {
        Ok(HMIRValue::new(
            ty,
            HMIRImmediate::Address(HMIRAddress::place(place)),
        ))
    };
    let reference = match value.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Address(address)) => {
            return Ok(HMIRValue::new(ty, HMIRImmediate::Address(address.clone())));
        }
        HMIRValueKind::Reference(local @ HMIRReference::Local(slot)) => {
            match context.runtime().slot(*slot).binding() {
                MIRBindable::Place(place_id) if context.env().pointer_inner(ty).is_some() => {
                    let place_id = *place_id;
                    let (out, result) = output(context, ty, span)?;
                    context.runtime_mut().emit_intrinsic(
                        MIRInternalIntrinsic::PlaceAddress {
                            out,
                            place: place_id,
                        },
                        span.clone(),
                    );
                    return Ok(result);
                }
                MIRBindable::Place(_) => return place(*local),
                MIRBindable::Register(_) if context.env().is_reference(value.ty()) => value,
                MIRBindable::Register(_) => unreachable!("a register has no address"),
            }
        }
        HMIRValueKind::Reference(other) => return place(*other),
        HMIRValueKind::Indirect(pointer) => pointer,
        HMIRValueKind::Immediate(_) => unreachable!("only storage has an address"),
    };

    let reference = super::value(context, reference, span)?;
    let (out, result) = output(context, ty, span)?;
    context.runtime_mut().emit_intrinsic(
        MIRInternalIntrinsic::ReferenceAddress { out, reference },
        span.clone(),
    );

    Ok(result)
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
    base: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = super::value(context, &pointer, span)?;
    if context.env().settings().assertions() {
        null_check(context, base.unwrap_or(&pointer), span)?;
    }

    let reference_ty = types::reference(context.env_mut(), inner, None, span);
    let target_ty = lower_type(context.env_mut(), reference_ty, span)?;
    let (out, reference) = output(context, reference_ty, span)?;
    context.runtime_mut().emit_intrinsic(
        MIRInternalIntrinsic::Bitcast {
            out,
            value,
            target_ty,
        },
        span.clone(),
    );

    Ok(HMIRValue::indirect(inner, reference))
}

pub fn null_check(
    context: &mut LocalLoweringContext,
    pointer: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    let checked = super::value(context, pointer, span)?;
    let null = MIRConstant::Nullptr {
        ty: lower_type(context.env_mut(), pointer.ty(), span)?,
    };
    let bool_ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let condition = register(context, bool_ty, span)?;
    let condition = context.runtime().register(condition);
    let check = MIRPtrIntrinsic::Neq {
        out: MIRTarget::Register(condition),
        lhs: checked,
        rhs: MIRValue::Constant(null),
    };
    context.runtime_mut().emit_intrinsic(check, span.clone());
    let message = Some("dereferenced a null pointer".into());
    context.runtime_mut().emit_intrinsic(
        MIRInternalIntrinsic::Assert {
            condition: MIRValue::Register(condition),
            message,
        },
        span.clone(),
    );
    Ok(())
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
    let tagged = matches!(
        context.env().resolve_kind(stored),
        HMIRTypeKind::TaggedUnion { .. }
    );
    let aggregate_ty = lower_type(context.env_mut(), stored, span)?;
    let base = if context.env().is_reference(base.ty()) {
        read(context, base, span)?
    } else {
        super::value(context, base, span)?
    };

    let reference_ty = types::reference(context.env_mut(), ty, bitfield, span);
    let (out, reference) = output(context, reference_ty, span)?;
    let projection = match tagged {
        true => MIRAggregateIntrinsic::SumVariant {
            out,
            base,
            variant: index,
            sum_ty: aggregate_ty,
        },
        false => MIRAggregateIntrinsic::StructField {
            out,
            base,
            field: index,
            struct_ty: aggregate_ty,
        },
    };
    context.runtime_mut().emit_intrinsic(projection, span.clone());

    Ok(HMIRValue::indirect(ty, reference))
}
