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

pub fn analyze(unit: &MIRUnit, options: MIRAnalysisOptions) -> CXResult<()> {
    AnalysisEnvironment::new(unit, options)
        .analyze(unit)
}
