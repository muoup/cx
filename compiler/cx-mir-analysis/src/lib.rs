use cx_log::CXResult;
use cx_mir::MIRUnit;

use crate::{
    framework::{environment::AnalysisEnvironment, pipeline::Pipeline},
    options::MIRAnalysisOptions,
};

mod framework;
mod log;
mod options;
mod passes;

pub fn analyze<'mir>(unit: &MIRUnit<'mir>, options: MIRAnalysisOptions) -> CXResult<()> {
    for function in unit.functions() {
        AnalysisEnvironment::new(unit, function, options.clone())
            .analyze()?;
    }

    Ok(())
}
