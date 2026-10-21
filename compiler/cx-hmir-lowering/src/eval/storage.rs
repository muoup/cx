use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{emit, env::function::LocalLoweringContext, value::HMIRValue};

// Writes a prepared value into storage that holds none yet and marks it live
pub(crate) fn initialize(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    emit::write(context, target, value, span)?;
    emit::initialize(context, target, span);

    Ok(())
}

// Scratch storage holding a value, for a value that needs an address
pub(crate) fn temporary(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let storage = emit::scratch(context, value.ty(), span)?;
    initialize(context, &storage, &value, span)?;

    Ok(storage)
}
