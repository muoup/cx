use cx_lmir::LMIRUnit;
use cx_log::CXResult;
use cx_mir::MIRUnit;

use crate::lowering::lower_unit;

mod context;
mod lowering;

pub fn generate_lmir(mir: &MIRUnit<'_>) -> CXResult<LMIRUnit> {
    lower_unit(mir)
}
