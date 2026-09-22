use cx_lmir::LMIRUnit;
use cx_log::CXResult;
use cx_mir::MIRUnit;

// TODO: The previous MIR-to-LMIR lowering still targets the removed MIR instruction hierarchy.
/*
mod context;
mod lowering;
*/

pub fn generate_lmir(_mir: &MIRUnit<'_>) -> CXResult<LMIRUnit> {
    todo!("MIR-to-LMIR lowering was removed with the MIR representation migration")
}
