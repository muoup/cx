use cx_log::{
    catalogue::ErrorDefinition,
    error::{CXError, context::CXInternalContext},
};

pub(crate) fn pipeline_error<T>(definition: &ErrorDefinition<T>, args: T) -> CXError {
    CXError::new(
        definition.bind(args),
        CXInternalContext::error("pipeline operation failed outside source context"),
    )
}
