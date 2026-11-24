use chl_hmir::ty::context::HMIRTypeContext;
use chl_log::CHLResult;
use chl_mir::expr::instruction::MIRInvalidationKind;
use chl_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::types,
    lower::types::lower_type,
    value::{self, HMIRValue, place::HMIRPlaceID},
};

pub(crate) fn initialize(
    context: &mut LocalLoweringContext,
    storage: &HMIRValue,
    contents: Option<&HMIRValue>,
    span: &TokenRange,
) -> CHLResult<()> {
    let place = value::as_place(storage).expect("fresh storage has a place");
    emit::storage::initialize(context, place, span)?;
    if let Some(value) = contents {
        emit::storage::write(context, storage, value, span)?;
    }
    Ok(())
}

pub(crate) fn tracked(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    span: &TokenRange,
) -> CHLResult<Option<HMIRPlaceID>> {
    let Some(place) = value::as_place(target) else {
        return Ok(None);
    };
    if types::bitfield(context.env(), target.ty()).is_some() {
        return Ok(None);
    }
    let ty = context.env().referent(target.ty());
    let tracked = if context.is_runtime() {
        let runtime = context.runtime();
        if place.brand() != runtime.brand() {
            return Ok(None);
        }
        let decl = runtime.builder().place(runtime.place(place));
        let (stored, nodrop) = (decl.ty(), decl.nodrop());
        let needs_nodrop = context.env().is_nodrop(ty);
        let ty = lower_type(context.env_mut(), ty, span)?;
        let module = context.env().module();
        needs_nodrop == nodrop
            && module.resolve_type(stored).kind() == module.resolve_type(ty).kind()
    } else {
        context
            .comptime()
            .slot(place)
            .is_some_and(|slot| context.env().same_type(slot.ty(), ty))
    };
    Ok(tracked.then_some(place))
}

pub(crate) fn drop(
    context: &mut LocalLoweringContext,
    place: HMIRPlaceID,
    span: &TokenRange,
) -> CHLResult<()> {
    emit::storage::invalidate(context, place, MIRInvalidationKind::Drop, span)
}

pub(crate) fn drop_scope(
    context: &mut LocalLoweringContext,
    index: usize,
    span: &TokenRange,
) -> CHLResult<()> {
    let places = context.runtime().scopes().control(index).places().to_vec();
    for place in places.into_iter().rev() {
        drop(context, place, span)?;
    }
    Ok(())
}

pub(crate) fn close_scope(context: &mut LocalLoweringContext, span: &TokenRange) -> CHLResult<()> {
    if context.is_runtime() {
        let index = context.runtime().scopes().top();
        drop_scope(context, index, span)?;
    }
    emit::close_scope(context);
    Ok(())
}
