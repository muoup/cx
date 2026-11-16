use chl_log::catalogue::ErrorDefinition;
use chl_log::error::CHLRawError;

pub(crate) fn raw<A>(definition: &ErrorDefinition<A>, args: A) -> CHLRawError {
    definition.bind(args)
}
