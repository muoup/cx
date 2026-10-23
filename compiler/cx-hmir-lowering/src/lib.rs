mod eval;
mod emit;
mod lower;
mod log;
mod symbol;

pub mod env;
pub mod value;

use cx_hmir::HMIRUnit;
use cx_log::CXResult;
use cx_mir::MIRUnit;

use crate::{env::HMIREnvironment, lower::lower_unit};

pub fn generate_mir<'hmir>(
    unit: &'hmir HMIRUnit,
    mut env: HMIREnvironment<'_, 'hmir>,
) -> CXResult<MIRUnit> {
    lower_unit(unit, &mut env)?;

    Ok(env.finish())
}
