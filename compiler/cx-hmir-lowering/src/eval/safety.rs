use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::function::{LocalLoweringContext, Permission},
    log::hmir_error,
};

pub(crate) fn require_nonsafe(
    context: &LocalLoweringContext,
    span: &TokenRange,
    operation: &str,
) -> CXResult<()> {
    if context.permission() < Permission::Nonsafe {
        return Err(hmir_error(
            span,
            &typecheck::UNSAFE_OPERATION,
            operation.into(),
        ));
    }
    Ok(())
}

pub(crate) fn require_call(
    context: &LocalLoweringContext,
    span: &TokenRange,
    safe: bool,
    source_defined: bool,
) -> CXResult<()> {
    if !safe {
        require_nonsafe(context, span, "call to non-safe function")?;
    }
    if source_defined {
        require_pure(context, span, "call to a user-defined function")?;
    }
    Ok(())
}

pub(crate) fn require_pure(
    context: &LocalLoweringContext,
    span: &TokenRange,
    operation: &str,
) -> CXResult<()> {
    if !context.expectations().allows_impure() {
        return Err(hmir_error(
            span,
            &typecheck::IMPURE_OPERATION,
            operation.into(),
        ));
    }
    Ok(())
}
