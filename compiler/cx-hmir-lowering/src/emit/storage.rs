use cx_hmir::{body::HMIRLocalID, ty::HMIRTypeID};
use cx_log::CXResult;
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use super::{comptime, unsupported};
use crate::{
    env::function::{FnContext, LocalLoweringContext},
    lower::mir,
    value::HMIRValue,
};

// Fresh, uninitialized storage of type 'ty' declared in the current scope, which carries the drop obligations of
// its type once initialized; binds no local and writes nothing
pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => mir::allocate(context, ty, name, span),
        FnContext::Comptime(_) => comptime::allocate(context, ty, name, span),
    }
}

// Fresh, uninitialized storage of type 'ty' that only carries an expression value, such as a merged result or a
// temporary that needs an address. Its scope is never opened, so it carries no obligations and is never dropped
pub fn scratch(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => mir::scratch(context, ty, span),
        FnContext::Comptime(_) => comptime::allocate(context, ty, None, span),
    }
}

pub fn adopt(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    source: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    match context.context() {
        FnContext::Runtime(_) => mir::adopt(context, target, source, span),
        FnContext::Comptime(_) => unsupported(span, "'@adopt'"),
    }
}

pub fn scratch_held(context: &LocalLoweringContext, value: &HMIRValue) -> bool {
    match context.context() {
        FnContext::Runtime(_) => mir::scratch_held(context, value),
        FnContext::Comptime(_) => false,
    }
}

pub fn place_held(context: &LocalLoweringContext, value: &HMIRValue) -> bool {
    match context.context() {
        FnContext::Runtime(_) => mir::place_held(context, value),
        FnContext::Comptime(_) => false,
    }
}

// Binds a local to an existing value; the value is not copied, moved or consumed, and the local is not marked
pub fn bind_local(context: &mut LocalLoweringContext, local: HMIRLocalID, value: HMIRValue) {
    match context.context_mut() {
        FnContext::Runtime(context) => context.scopes_mut().bind(local, value),
        FnContext::Comptime(context) => context.scopes_mut().bind(local, value),
    }
}

// The contents of a value, copied into a value of their own; the source is not checked for being copyable, so
// the caller authorizes the copy
pub fn copy(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => mir::copy(context, value, span),
        FnContext::Comptime(_) => comptime::copy(context, value, span),
    }
}

// The current contents of the place a reference names, captured without copying them; the place stays live, so a
// move out of it is a separate invalidation
pub fn lift(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    origin: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => mir::lift(context, value, origin, span),
        FnContext::Comptime(_) => comptime::copy(context, value.clone(), span),
    }
}

// Stores the payload of 'value' into the storage of 'target' and does nothing else: the old contents are not
// dropped, the target is not marked initialized and the source is not invalidated
pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    match context.context() {
        FnContext::Runtime(_) => mir::write(context, target, value, span),
        FnContext::Comptime(_) => comptime::write(context, target, value, span),
    }
}

// Writes 'value', when given, into 'target' and marks the storage as holding a live value. Storage behind a
// pointer is untracked and a register is live once written, so neither is marked; a comptime slot tracks byte
// initialization separately, so bytes that were never written stay unreadable
pub fn initialize(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<()> {
    if let Some(value) = value {
        write(context, target, value, span)?;
    }
    match context.context() {
        FnContext::Runtime(_) => mir::initialize(context, target, span),
        FnContext::Comptime(_) => comptime::initialize(context, target, span),
    }
    Ok(())
}

// Scratch storage holding a value, for a value that needs an address
pub fn temporary(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let storage = scratch(context, value.ty(), span)?;
    initialize(context, &storage, Some(&value), span)?;

    Ok(storage)
}

// Ends the liveness of the place a value names as a move, drop or leak; registers and storage behind a pointer
// are untracked. Comptime records this on the concrete slot only: there is no flow analysis over it and a dropped
// nodrop value is not reported
pub fn invalidate(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    kind: MIRInvalidationKind,
    span: &TokenRange,
) {
    match context.context() {
        FnContext::Runtime(_) => mir::invalidate(context, value, kind, span),
        FnContext::Comptime(_) => comptime::invalidate(context, value, kind, span),
    }
}
