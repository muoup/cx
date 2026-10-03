use cx_log::{
    CXResult,
    catalogue::ErrorDefinition,
    error::{
        CXError,
        context::{CXNoSourceContext, CXPointingContext},
    },
};
use std::path::Path;

pub(crate) fn point_error<T, A>(
    path: &Path,
    byte_index: usize,
    definition: &ErrorDefinition<A>,
    args: A,
) -> CXResult<T> {
    Err(CXError::new(
        definition.bind(args),
        CXPointingContext::error(path.to_path_buf(), byte_index),
    ))
}

pub(crate) fn no_source_error<A>(
    definition: &ErrorDefinition<A>,
    args: A,
    context: &'static str,
) -> CXError {
    CXError::new(definition.bind(args), CXNoSourceContext::error(context))
}