use cx_hmir::ty::{HMIRTypeID, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_mir::{
    MIRBindable, MIRInstructionKind, MIRPlaceDecl, MIRPlaceID, MIRScopeID, MIRTarget,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use super::{location, output, reference, store};
use crate::{
    env::function::LocalLoweringContext,
    eval::types,
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
};

fn allocate_place(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<MIRPlaceID> {
    let nodrop = context.env().is_nodrop(ty);
    let ty = types::lower_type(context.env_mut(), ty, span)?;
    let runtime = context.runtime_mut();
    let scope = *runtime
        .open_scopes()
        .last()
        .expect("Place allocated outside of a scope");

    Ok(runtime
        .builder_mut()
        .allocate_place(MIRPlaceDecl::new(ty, name, scope, nodrop, false)))
}

// Registers a MIR binding as a fresh evaluation slot; this names storage and never binds a local
pub fn slot(context: &mut LocalLoweringContext, ty: HMIRTypeID, bindable: MIRBindable) -> HMIRValue {
    let slot = context.runtime_mut().allocate_slot(bindable);

    HMIRValue::place(ty, HMIRPlace::Local(slot))
}

fn bindable(context: &LocalLoweringContext, value: &HMIRValue) -> Option<MIRBindable> {
    match value.kind() {
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => Some(context.runtime().slot(*slot).clone()),
        HMIRValueKind::PlaceRef(HMIRPlace::Global(_)) => {
            todo!("global places: globals are not lowered yet")
        }
        HMIRValueKind::Constant(_) | HMIRValueKind::Indirect(_) => None,
    }
}

// Where a value's storage can be written: a place, or storage behind a pointer; registers have no address
pub(super) fn target(context: &mut LocalLoweringContext, value: &HMIRValue) -> Option<MIRTarget> {
    match value.kind() {
        HMIRValueKind::Indirect(pointer) => Some(MIRTarget::Indirect(reference(context, pointer))),
        _ => match bindable(context, value)? {
            MIRBindable::Place(place) => Some(MIRTarget::Place(place)),
            MIRBindable::Register(_) => None,
        },
    }
}

pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let place = allocate_place(context, ty, name, span)?;

    Ok(slot(context, ty, MIRBindable::Place(place)))
}

pub fn parameter(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let place = allocate_place(context, ty, name, span)?;
    context.runtime_mut().builder_mut().add_parameter(place);

    let value = slot(context, ty, MIRBindable::Place(place));
    initialize(context, &value, span);

    Ok(value)
}

pub fn addressable(context: &mut LocalLoweringContext, value: &HMIRValue) -> bool {
    target(context, value).is_some()
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
    if let Some(place) = bindable(context, value) {
        context
            .runtime_mut()
            .emit(MIRInstructionKind::Invalidate { place, kind }, span.clone());
    }
}

// Moves the cleanup of a place into 'scope', renaming it when given a name; its type, ownership and storage are
// kept. A register has no cleanup to move
pub fn relocate(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    scope: MIRScopeID,
    name: Option<CXIdent>,
) {
    let Some(MIRBindable::Place(place)) = bindable(context, value) else {
        return;
    };
    let decl = context
        .runtime_mut()
        .builder_mut()
        .place_mut(place)
        .expect("owned place exists");

    *decl = MIRPlaceDecl::new(
        decl.ty(),
        name.or_else(|| decl.debug_name().cloned()),
        scope,
        decl.nodrop(),
        decl.adopted(),
    );
}

pub fn read(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let shared = match value.kind() {
        HMIRValueKind::Constant(_) => true,
        _ => !addressable(context, &value) && !context.env().is_nocopy(value.ty()),
    };
    if shared {
        return Ok(value);
    }

    let ty = types::lower_type(context.env_mut(), value.ty(), span)?;
    let source = location(context, &value, span)?;
    let (out, result) = output(context, value.ty(), span)?;
    store(context, out, source, ty, span.clone());
    initialize(context, &result, span);

    Ok(result)
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    let Some(destination) = self::target(context, target) else {
        unreachable!("write to a value without an address");
    };
    let ty = types::lower_type(context.env_mut(), target.ty(), span)?;
    let source = location(context, value, span)?;
    store(context, destination, source, ty, span.clone());

    Ok(())
}
