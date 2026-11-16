use chl_lmir::LMIRUnit;
use chl_log::CHLResult;
use chl_mir::MIRUnit;

use crate::lowering::lower_unit;

mod context;
mod lowering;

pub fn generate_lmir(mir: &MIRUnit) -> CHLResult<LMIRUnit> {
    lower_unit(mir)
}
