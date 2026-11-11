pub mod float;
pub mod int;
pub mod intrinsics;
pub mod storage;

use cx_hmir::ty::context::HMIRTypeContext;
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    value::{HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarFault {
    ZeroDivisor,
    Undefined,
}

// What a value is at its own type: a reference or pointer is the address it holds, and a reference to storage used
// as a plain value is the contents of that storage
pub(crate) fn resolve(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValueKind> {
    match value.kind() {
        HMIRValueKind::Reference(_)
            if !context.env().is_reference(value.ty()) && !context.env().is_pointer(value.ty()) =>
        {
            Ok(storage::contents(context, value, span)?.take_kind())
        }
        kind => Ok(kind.clone()),
    }
}

// A value that has no address at its own type
pub(crate) fn immediate(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<Option<HMIRImmediate>> {
    Ok(match resolve(context, value, span)? {
        HMIRValueKind::Immediate(immediate) => Some(immediate),
        _ => None,
    })
}
