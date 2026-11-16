mod eval;
mod emit;
mod lower;
mod log;

pub mod env;
pub mod value;

use chl_hmir::HMIRUnit;
use chl_log::CHLResult;

use crate::{env::HMIREnvironment, lower::lower_unit};

pub fn generate_mir<'hmir>(
    unit: &'hmir HMIRUnit,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CHLResult<()> {
    lower_unit(unit, env)
}
