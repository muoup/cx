use cx_log::CXResult;
use cx_mir::MIRUnit;

use crate::framework::{environment::AnalysisEnvironment, pipeline::Pipeline};

mod framework;
mod passes;

pub(crate) mod log;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MIRAnalysisOptions {
    pub liveness: bool,
    pub values: bool,
}

impl Default for MIRAnalysisOptions {
    fn default() -> Self {
        Self {
            liveness: true,
            values: true,
        }
    }
}

pub fn analyze<'mir>(unit: &MIRUnit<'mir>, options: MIRAnalysisOptions) -> CXResult<()> {
    for (_, function) in unit.functions() {
        let Some(body) = function.body() else {
            continue;
        };

        AnalysisEnvironment::new(unit, function, body, options).analyze()?;
    }

    Ok(())
}
