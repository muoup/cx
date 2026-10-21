use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_mir::{
    MIRBindable, MIRInstructionKind, MIRPlaceDecl, MIRScopeID, MIRTarget,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use super::{global, lower_value, output, reference, register, store};
use crate::{
    env::function::LocalLoweringContext,
    eval::types,
    value::{HMIRPlace, HMIRValue, HMIRValueKind, constant::HMIRConstant},
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
    let slot = context.runtime_mut().allocate_slot(bindable);

    HMIRValue::place(ty, HMIRPlace::Local(slot))
}

fn bindable(context: &LocalLoweringContext, value: &HMIRValue) -> Option<MIRBindable> {
    let is_reference = context.env().is_reference(value.ty());
    let place = match value.kind() {
        HMIRValueKind::PlaceRef(place) => place,
        HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. })
            if is_reference && path.is_empty() =>
        {
            place
        }
        _ => return None,
    };
    let HMIRPlace::Local(slot) = place else {
        return None;
    };

    match context.runtime().slot(*slot) {
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
            global::reference(context, *def, value.ty(), span)?,
        )),
        HMIRValueKind::Constant(HMIRConstant::Address { place: HMIRPlace::Global(def), path, .. })
            if is_reference && path.is_empty() =>
        {
            let inner = context.env().reference_inner(value.ty()).unwrap();
            Some(MIRTarget::Global(global::reference(context, *def, inner, span)?))
        }
        HMIRValueKind::Indirect(pointer) if !is_reference => {
            Some(MIRTarget::Indirect(reference(context, pointer)))
        }
        HMIRValueKind::Indirect(_) => todo!("write to a stored reference"),
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) if is_reference => {
            match context.runtime().slot(*slot) {
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
    let scope = *context
        .runtime()
        .open_scopes()
        .last()
        .expect("Place allocated outside of a scope");

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
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some(source @ MIRBindable::Place(_)) = bindable(context, value) else {
        unreachable!("only a place is lifted")
    };
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        unreachable!("a lift reads through a reference")
    };
    let out = register(context, inner, span)?;
    context.runtime_mut().emit(
        MIRInstructionKind::Lift {
            out,
            source: source.clone(),
            origin: source,
        },
        span.clone(),
    );
    let runtime = context.runtime_mut();
    let lifted = runtime.allocate_slot(MIRBindable::Register(out));
    runtime.add_lift(lifted);

    Ok(HMIRValue::place(inner, HMIRPlace::Local(lifted)))
}

pub fn copy(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let runtime_value = lower_value(context, &value, span)?;

    // Keep this as not a reference_inner because we need the bitfield data later
    let HMIRTypeKind::ReferenceTo(result_type) = context.env().resolve_type_id(value.ty()).kind()
    else {
        unreachable!("read from type: {}", types::name(context.env(), value.ty()))
    };
    let result_type = *result_type;

    let ty = types::lower_type(context.env_mut(), result_type, span)?;
    let (out, result) = output(context, result_type, span)?;
    context.runtime_mut().emit(
        MIRInstructionKind::Store {
            target: out,
            value: runtime_value,
            ty,

            // TODO: Implement
            bitfield: None,
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
    let ty = types::lower_type(context.env_mut(), stored, span)?;
    let source = lower_value(context, value, span)?;
    store(context, destination, source, ty, span.clone());

    Ok(())
}
