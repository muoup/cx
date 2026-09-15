use cx_mir::visit::MIRWalk;
use cx_mir::{MIRComptimeInstrKind, MIRInstrKind};

pub(crate) trait AnalysisInstruction: MIRWalk + cx_mir::MIRInstructionKind {
    fn standard(&self) -> Option<&MIRInstrKind>;
}

impl AnalysisInstruction for MIRInstrKind {
    fn standard(&self) -> Option<&MIRInstrKind> {
        Some(self)
    }
}

impl AnalysisInstruction for MIRComptimeInstrKind {
    fn standard(&self) -> Option<&MIRInstrKind> {
        match self {
            Self::Standard(kind) => Some(kind),
            Self::Comptime(_) => None,
        }
    }
}
