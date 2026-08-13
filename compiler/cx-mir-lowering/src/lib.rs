use cx_lmir::LMIRUnit;
use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::registry::THIRDecomposedRegistry;

mod lower;
mod typing;

pub fn generate_lmir(mir: &MIRUnit, registry: &THIRDecomposedRegistry) -> CXResult<LMIRUnit> {
    lower::lower_unit(mir, registry)
}
