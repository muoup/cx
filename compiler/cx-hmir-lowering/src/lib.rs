mod deduce;
mod eval;
mod function;
mod lower;
mod module;
mod pattern;
mod program;
mod value;

mod log;

pub(crate) mod env;

use cx_hmir::HMIRUnit;
use cx_log::{
    CXResult,
    catalogue::ErrorDefinition,
    error::{CXError, context::from_token_range},
};
use cx_mir::MIRUnit;
use cx_namespace::module::QualifiedName;
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;

use crate::{env::HMIREnvironment, module::lower_roots, program::Program};

pub fn generate_mir<'l>(mut env: HMIREnvironment) -> CXResult<MIRUnit> {
    // TODO

    Ok(env.finish())
}
