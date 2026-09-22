use cx_log::CXResult;
use cx_mir::MIRUnit;

// TODO: The ownership/value analysis passes still target the removed MIR instruction hierarchy.
/*
mod framework;
mod log;
mod options;
mod passes;
*/

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MIRAnalysisOptions {
    pub ownership: bool,
    pub values: bool,
}

impl Default for MIRAnalysisOptions {
    fn default() -> Self {
        Self {
            ownership: true,
            values: true,
        }
    }
}

pub struct Pipeline {
    _options: MIRAnalysisOptions,
}

impl Pipeline {
    pub fn new(options: MIRAnalysisOptions) -> Self {
        Self { _options: options }
    }

    pub fn analyze<'mir>(&mut self, _unit: &MIRUnit<'mir>) -> CXResult<()> {
        todo!("MIR analysis was removed with the MIR representation migration")
    }
}

pub fn analyze<'mir>(_unit: &MIRUnit<'mir>, _options: MIRAnalysisOptions) -> CXResult<()> {
    todo!("MIR analysis was removed with the MIR representation migration")
}
