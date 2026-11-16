use chl_log::{
    CHLResult,
    catalogue::ErrorDefinition,
    error::{
        CHLError,
        context::{CHLNoSourceContext, CHLPointingContext},
    },
};
use std::path::Path;

pub(crate) fn point_error<T, A>(
    path: &Path,
    byte_index: usize,
    definition: &ErrorDefinition<A>,
    args: A,
) -> CHLResult<T> {
    Err(CHLError::new(
        definition.bind(args),
        CHLPointingContext::error(path.to_path_buf(), byte_index),
    ))
}

pub(crate) fn no_source_error<A>(
    definition: &ErrorDefinition<A>,
    args: A,
    context: &'static str,
) -> CHLError {
    CHLError::new(definition.bind(args), CHLNoSourceContext::error(context))
}