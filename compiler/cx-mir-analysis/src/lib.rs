use cx_log::CXResult;
use cx_mir::MIRUnit;

mod ownership;

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
    options: MIRAnalysisOptions,
}

impl Pipeline {
    pub fn new(options: MIRAnalysisOptions) -> Self {
        Self { options }
    }

    pub fn analyze<'mir>(&mut self, unit: &MIRUnit<'mir>) -> CXResult<()> {
        if self.options.ownership {
            for function in unit.functions() {
                if let Some(body) = function.body() {
                    ownership::analyze(body)?;
                }
            }
        }
        Ok(())
    }
}

pub fn analyze<'mir>(unit: &MIRUnit<'mir>, options: MIRAnalysisOptions) -> CXResult<()> {
    Pipeline::new(options).analyze(unit)
}
