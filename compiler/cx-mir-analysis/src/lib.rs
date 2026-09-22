use cx_log::CXResult;
use cx_mir::MIRUnit;

use crate::framework::{environment::AnalysisEnvironment, pipeline::Pipeline};

mod framework;
mod passes;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MIRAnalysisOptions {
    pub ownership: bool,
    pub values: bool,
}

impl Default for MIRAnalysisOptions {
    fn default() -> Self {
        Self {
            ownership: true,
            values: false,
        }
    }
}

pub fn analyze<'mir>(unit: &MIRUnit<'mir>, options: MIRAnalysisOptions) -> CXResult<()> {
    for function in unit.functions() {
        if function.body().is_none() {
            continue;
        }
        
        AnalysisEnvironment::new(unit, function, options).analyze()?;
    }

    Ok(())
}
