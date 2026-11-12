mod eval;
mod emit;
mod lower;
mod log;

pub mod env;
pub mod value;

use cx_hmir::HMIRUnit;
use cx_log::CXResult;

use crate::{env::HMIREnvironment, lower::lower_unit};

pub fn generate_mir<'hmir>(
    unit: &'hmir HMIRUnit,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CXResult<()> {
    lower_unit(unit, env)
}
