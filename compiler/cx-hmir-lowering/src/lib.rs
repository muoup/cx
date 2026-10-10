mod deduce;
mod eval;
mod function;
mod lower;
mod pattern;

mod log;

pub mod env;

use cx_hmir::HMIRUnit;
use cx_log::CXResult;
use cx_mir::MIRUnit;

use crate::{env::HMIREnvironment, lower::lower_unit};

pub fn generate_mir<'l>(unit: &HMIRUnit, mut env: HMIREnvironment) -> CXResult<MIRUnit> {
    lower_unit(unit, &mut env);

    Ok(env.finish())
}
