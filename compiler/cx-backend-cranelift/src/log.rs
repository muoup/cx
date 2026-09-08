use cx_log::catalogue::ErrorDefinition;
use cx_log::error::CXRawError;

pub(crate) fn raw<A>(definition: &ErrorDefinition<A>, args: A) -> CXRawError {
    definition.bind(args)
}
