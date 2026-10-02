mod deduce;
mod eval;
mod function;
mod lower;
mod module;
mod program;
mod ty;
mod value;

use cx_hmir::HMIRUnit;
use cx_log::{
    CXResult,
    catalogue::mir,
    error::{CXError, context::from_token_range},
};
use cx_mir::MIRUnit;
use cx_namespace::module::QualifiedName;
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;

use crate::{module::lower_roots, program::Program};

// Stages an HMIR unit into MIR; 'load' supplies single-def units for names the unit imports
pub fn generate_mir<'l>(
    unit: HMIRUnit,
    load: impl FnMut(&QualifiedName) -> Option<HMIRUnit> + 'l,
    architecture: ArchitectureConfig,
    require_explicit_return: bool,
) -> CXResult<MIRUnit<'static>> {
    let mut program = Program::new(unit, Box::new(load), architecture, require_explicit_return);
    lower_roots(&mut program)?;
    let (types, module) = program.into_parts();
    Ok(module.finish(types.finish()))
}

pub(crate) fn staging_error(span: &TokenRange, message: String) -> CXError {
    CXError::new(mir::HMIR_STAGING.bind(message), from_token_range(span))
}
