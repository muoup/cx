use chl_log::{
    catalogue::ErrorDefinition,
    error::{CHLError, context::CHLNoSourceContext},
};

pub(crate) fn pipeline_error<T>(definition: &ErrorDefinition<T>, args: T) -> CHLError {
    CHLError::new(
        definition.bind(args),
        CHLNoSourceContext::error("pipeline operation failed outside source context"),
    )
}
