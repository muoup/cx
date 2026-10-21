use cx_hmir::{body::HMIRLocalID, ty::HMIRTypeID};
use cx_log::CXResult;
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use super::{
    comptime, runtime,
    transfer::{self, Transfer},
};
use crate::{
    env::function::{FnContext, LocalLoweringContext},
    value::HMIRValue,
};

// Fresh, uninitialized storage of type 'ty' in the current scope; binds no local, writes and consumes nothing
pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::allocate(context, ty, name, span),
        FnContext::Comptime(_) => Ok(comptime::allocate(context, ty)),
    }
}

// Binds a local to an existing value; the value is not copied, moved or consumed, and the local is not marked
pub fn bind_local(context: &mut LocalLoweringContext, local: HMIRLocalID, value: HMIRValue) {
    match context.context_mut() {
        FnContext::Runtime(context) => context.set_local(local, value),
        FnContext::Comptime(context) => context.set_local(local, value),
    }
}

// The contents of a value, copied into initialized storage of their own; the source is neither consumed nor
// checked for being copyable, so the caller authorizes the copy. Constants, and registers of a copyable type,
// cannot be written and are returned as they are
pub fn copy(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::copy(context, value, span),
        FnContext::Comptime(_) => comptime::copy(context, value),
    }
}

// Stores the payload of 'value' into the storage of 'target' and does nothing else: the old contents are not
// dropped, the target is not marked initialized and the source is not consumed
pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    match context.context() {
        FnContext::Runtime(_) => runtime::write(context, target, value, span),
        FnContext::Comptime(_) => comptime::write(context, target, value),
    }
}

// Marks the storage of 'value' as holding a live value. Storage behind a pointer is untracked and a register is
// live once written, so neither is marked; a comptime slot is live exactly when it holds a payload, so there this
// only asserts that a write preceded it
pub fn initialize(context: &mut LocalLoweringContext, value: &HMIRValue, span: &TokenRange) {
    match context.context() {
        FnContext::Runtime(_) => runtime::initialize(context, value, span),
        FnContext::Comptime(_) => comptime::initialize(context, value),
    }
}

// Ends the liveness of the slot a value names, place or register, as a move, drop or leak; storage behind a
// pointer is untracked. Comptime records this on the concrete slot only: there is no flow analysis over it and a
// dropped nodrop value is not reported
pub fn invalidate(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    kind: MIRInvalidationKind,
    span: &TokenRange,
) {
    match context.context() {
        FnContext::Runtime(_) => runtime::invalidate(context, value, kind, span),
        FnContext::Comptime(_) => comptime::invalidate(context, value, kind),
    }
}

pub fn spill(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let place = allocate(context, value.ty(), None, span)?;
    transfer::transfer(context, &place, value, Transfer::Init, span)?;

    Ok(place)
}
