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
