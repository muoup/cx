// TODO

use cx_log::CXResult;

use crate::framework::{environment::AnalysisEnvironment, pipeline::AnalysisPass};

pub struct ValueTracking;

impl AnalysisPass for ValueTracking {
    fn function_entry(
        &mut self,
        _env: &AnalysisEnvironment,
    ) -> CXResult<()> {
        Ok(())
    }

    fn analyze_instruction(
        &mut self,
        _env: &AnalysisEnvironment,
        _instruction: &cx_mir::MIRInstruction,
    ) -> CXResult<()> {
        Ok(())
    }

    fn merge(
        &mut self,
        _env: &AnalysisEnvironment,
        _other: cx_mir::MIRBasicBlockID,
    ) -> CXResult<bool> {
        Ok(false)
    }

    fn reload_block(
        &mut self,
        _env: &AnalysisEnvironment,
        _block: cx_mir::MIRBasicBlockID,
    ) -> CXResult<()> {
        Ok(())
    }
}
