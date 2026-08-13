use cx_lmir::LMIRUnit;
use cx_log::CXResult;
use cx_mir::MIRUnit;

mod lower;
mod typing;

pub fn generate_lmir(mir: &MIRUnit) -> CXResult<LMIRUnit> {
    lower::lower_unit(mir, &mir.types)
}
