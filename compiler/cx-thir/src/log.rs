use cx_log::{CXRawResult, catalogue::ErrorDefinition};

pub(crate) fn log_error<T, U>(definition: &ErrorDefinition<T>, args: T) -> CXRawResult<U> {
    Err(definition.bind(args))
}
