use crate::{
    MIRBody, MIRInstruction, MIRInstrKind, MIRInstructionKind, MIRRegister, MIRStagedTargets,
    MIRStagedTemplate, MIRValue,
};
use std::sync::Arc;

pub type MIRComptimeBody = MIRBody<MIRComptimeInstrKind>;
pub type MIRComptimeInstr = MIRInstruction<MIRComptimeInstrKind>;

#[derive(Debug, Clone)]
pub enum MIRComptimeOp {
    Call {
        out: Option<MIRRegister>,
        callee: MIRValue,
        args: Vec<MIRValue>,
    },
    MakeStaged {
        out: MIRRegister,
        template: Arc<MIRStagedTemplate>,
        captures: Vec<MIRValue>,
    },
    ApplyStaged {
        out: Option<MIRRegister>,
        staged: MIRValue,
        args: Vec<MIRValue>,
        targets: MIRStagedTargets,
    },
}

#[derive(Debug, Clone)]
pub enum MIRComptimeInstrKind {
    Standard(MIRInstrKind),
    Comptime(MIRComptimeOp),
}

impl MIRInstructionKind for MIRComptimeInstrKind {
    fn is_terminator(&self) -> bool {
        match self {
            Self::Standard(kind) => kind.is_terminator(),
            Self::Comptime(_) => false,
        }
    }
}

impl From<MIRInstrKind> for MIRComptimeInstrKind {
    fn from(kind: MIRInstrKind) -> Self {
        Self::Standard(kind)
    }
}

impl From<MIRComptimeOp> for MIRComptimeInstrKind {
    fn from(kind: MIRComptimeOp) -> Self {
        Self::Comptime(kind)
    }
}
