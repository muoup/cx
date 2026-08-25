use cx_lmir::LMIRUnit;
use cx_log::CXResult;
use cx_mir::MIRUnit;

mod context;
mod lowering;

pub fn generate_lmir(mir: &MIRUnit) -> CXResult<LMIRUnit> {
    lowering::lower_unit(mir, mir.types())
}
