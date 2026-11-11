use cx_hmir::{body::HMIRLocalID, ty::HMIRTypeID};
use cx_log::CXResult;
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit::{comptime, runtime, unsupported},
    env::function::{FnContext, LocalLoweringContext},
    value::HMIRValue,
};

// Fresh, uninitialized storage of type 'ty' declared in the current scope, referenced as 'ty&'; it carries the drop
// obligations of its type once initialized, and binds no local
pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::storage::allocate(context, ty, name, span),
        FnContext::Comptime(_) => comptime::storage::allocate(context, ty, name, span),
    }
}

// Fresh, uninitialized storage of type 'ty' that only carries an expression value, such as a merged result or a
// temporary that needs an address; it belongs to no scope, so it carries no obligations and is never dropped
pub fn scratch(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::storage::scratch(context, ty, span),
        FnContext::Comptime(_) => comptime::storage::allocate(context, ty, None, span),
    }
}

// Storage of type 'ty' over the memory the reference 'source' designates, declared in the current scope; it takes over
// the obligations of that memory
pub fn adopt(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    source: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::storage::adopt(context, ty, name, source, span),
        FnContext::Comptime(_) => unsupported(span, "'@adopt'"),
    }
}

// Binds a local to an existing value; the value is not copied, moved or consumed, and the local is not marked
pub fn bind_local(context: &mut LocalLoweringContext, local: HMIRLocalID, value: HMIRValue) {
    match context.context_mut() {
        FnContext::Runtime(context) => context.scopes_mut().bind(local, value),
        FnContext::Comptime(context) => context.scopes_mut().bind(local, value),
    }
}

// The contents of the storage a reference designates, copied out as they are now; the source is not checked for
// being copyable, so the caller authorizes the copy
pub fn read(
    context: &mut LocalLoweringContext,
    source: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::storage::read(context, source, span),
        FnContext::Comptime(_) => comptime::storage::read(context, source, span),
    }
}

// The current contents of the place a reference names, taken without copying them; the place stays live, so a move
// out of it is a separate invalidation
pub fn lift(
    context: &mut LocalLoweringContext,
    source: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime::storage::lift(context, source, span),
        FnContext::Comptime(_) => comptime::storage::read(context, source, span),
    }
}

// Stores 'value' into the storage 'target' references and does nothing else: the old contents are not dropped, the
// target is not marked initialized and the source is not invalidated
pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    match context.context() {
        FnContext::Runtime(_) => runtime::storage::write(context, target, value, span),
        FnContext::Comptime(_) => comptime::storage::write(context, target, value, span),
    }
}

// Writes 'value', when given, into 'target' and marks the storage as holding a live value. Storage behind a
// pointer is untracked, so it is not marked; a comptime slot tracks byte initialization separately, so bytes that
// were never written stay unreadable
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
        FnContext::Runtime(_) => runtime::storage::initialize(context, target, span),
        FnContext::Comptime(_) => comptime::storage::initialize(context, target, span),
    }
    Ok(())
}

// Ends the liveness of the place a reference names as a move, drop or leak; storage behind a pointer is untracked.
// Comptime records this on the concrete slot only: there is no flow analysis over it and a dropped nodrop value is
// not reported
pub fn invalidate(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    kind: MIRInvalidationKind,
    span: &TokenRange,
) {
    match context.context() {
        FnContext::Runtime(_) => runtime::storage::invalidate(context, target, kind, span),
        FnContext::Comptime(_) => comptime::storage::invalidate(context, target, kind, span),
    }
}
