mod assertions;
mod ownership;
mod types;

pub use types::{MIRAnalysisError, MIRAnalysisOptions};

use cx_mir::MIRUnit;

pub fn analyze(unit: &MIRUnit, options: MIRAnalysisOptions) -> Result<(), MIRAnalysisError> {
    ownership::check(unit)?;

    if options.check_assertions {
        assertions::check(unit)?;
    }

    Ok(())
}
