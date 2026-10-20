use cx_hmir::ty::context::HMIRTypeContext;
use cx_log::CXResult;
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;

use super::storage;
use crate::{
    env::function::LocalLoweringContext,
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Transfer {
    // The target holds no value yet
    Init,
    // The target holds a live value, which is dropped
    Replace,
}

// Moves 'value' into the storage of 'target', in order: a Replace first reads a value behind a pointer or in a
// global into storage of its own, since those may share the overwritten storage, then drops the old contents;
// the payload is written, the target marked initialized, and the source consumed once. Transferring a slot into
// itself does nothing, so it is neither dropped nor consumed
pub fn transfer(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: HMIRValue,
    mode: Transfer,
    span: &TokenRange,
) -> CXResult<()> {
    if let (HMIRValueKind::PlaceRef(target), HMIRValueKind::PlaceRef(source)) =
        (target.kind(), value.kind())
        && target == source
    {
        return Ok(());
    }

    let value = match (mode, value.kind()) {
        (
            Transfer::Replace,
            HMIRValueKind::Indirect(_) | HMIRValueKind::PlaceRef(HMIRPlace::Global(_)),
        ) => storage::read(context, value, span)?,
        _ => value,
    };
    if mode == Transfer::Replace {
        storage::invalidate(context, target, MIRInvalidationKind::Drop, span);
    }

    storage::write(context, target, &value, span)?;
    storage::initialize(context, target, span);
    consume(context, &value, span);

    Ok(())
}

// Ends the ownership of a value that was moved elsewhere. A reference is not consumed as its referent and storage
// behind a pointer is not owned; a register of a copyable type has no ownership to end
pub fn consume(context: &mut LocalLoweringContext, value: &HMIRValue, span: &TokenRange) {
    if context.env().reference_inner(value.ty()).is_some()
        || !matches!(value.kind(), HMIRValueKind::PlaceRef(HMIRPlace::Local(_)))
    {
        return;
    }
    if !storage::addressable(context, value) && !context.env().is_nocopy(value.ty()) {
        return;
    }

    storage::invalidate(context, value, MIRInvalidationKind::Move, span);
}
