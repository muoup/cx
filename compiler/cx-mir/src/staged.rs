use std::sync::Arc;

use crate::{
    MIRBasicBlock, MIRBody, MIRComptimeBody, MIRComptimeInstrKind, MIRComptimeOp, MIRInstr,
    MIRInstrKind, MIRInstructionKind, MIRPlaceID, MIRRegister, MIRStagedExitKind, MIRStagedTargets,
    MIRTypeID, MIRValue,
};

#[derive(Debug, Clone, Copy)]
pub enum MIRStagedCapture {
    Register(MIRRegister),
    Place(MIRPlaceID),
}

#[derive(Debug, Clone)]
pub struct MIRStagedTemplate {
    body: MIRStagedBody,
    captures: Arc<[MIRStagedCapture]>,
    params: Arc<[MIRRegister]>,
    result_type: MIRTypeID,
    diverges: bool,
}

impl MIRStagedTemplate {
    pub fn new(
        body: MIRStagedBody,
        captures: Vec<MIRStagedCapture>,
        params: Vec<MIRRegister>,
        result_type: MIRTypeID,
        diverges: bool,
    ) -> Self {
        Self {
            body,
            captures: captures.into(),
            params: params.into(),
            result_type,
            diverges,
        }
    }

    pub fn body(&self) -> &MIRStagedBody {
        &self.body
    }

    pub fn captures(&self) -> &[MIRStagedCapture] {
        &self.captures
    }

    pub fn params(&self) -> &[MIRRegister] {
        &self.params
    }

    pub fn result_type(&self) -> MIRTypeID {
        self.result_type
    }

    pub fn diverges(&self) -> bool {
        self.diverges
    }
}

pub type MIRStagedBody = MIRBody<MIRStagedInstrKind>;
pub type MIRStagedBasicBlock = MIRBasicBlock<MIRStagedInstrKind>;
pub type MIRStagedInstr = MIRInstr<MIRStagedInstrKind>;

#[derive(Debug, Clone)]
pub enum MIRStagedInstrKind {
    Standard(MIRInstrKind),
    Comptime(MIRComptimeOp),
    CallerReturn {
        value: Option<MIRValue>,
    },
    Move {
        out: MIRRegister,
        value: MIRValue,
    },
    ScopeExit {
        kind: MIRStagedExitKind,
    },
    Yield {
        value: Option<MIRValue>,
        ty: Option<MIRTypeID>,
    },
    Use {
        value: MIRValue,
        targets: MIRStagedTargets,
    },
}

impl From<MIRInstrKind> for MIRStagedInstrKind {
    fn from(kind: MIRInstrKind) -> Self {
        match kind {
            MIRInstrKind::Return { value } => Self::CallerReturn { value },
            kind => Self::Standard(kind),
        }
    }
}

impl From<MIRComptimeOp> for MIRStagedInstrKind {
    fn from(kind: MIRComptimeOp) -> Self {
        Self::Comptime(kind)
    }
}

impl MIRInstructionKind for MIRStagedInstrKind {
    fn is_terminator(&self) -> bool {
        match self {
            Self::Standard(kind) => kind.is_terminator(),
            Self::CallerReturn { .. }
            | Self::ScopeExit { .. }
            | Self::Yield { .. } => true,
            Self::Comptime(_) | Self::Move { .. } | Self::Use { .. } => false,
        }
    }
}

impl MIRStagedBody {
    pub fn into_runtime(self) -> Result<MIRBody, MIRStagedInstr> {
        self.try_map(|instruction| {
            let kind = match instruction.kind {
                MIRStagedInstrKind::Standard(kind) => kind,
                MIRStagedInstrKind::CallerReturn { value } => MIRInstrKind::Return { value },
                _ => return Err(instruction),
            };
            Ok(MIRInstr::new(kind, instruction.token_range))
        })
    }

    pub fn into_comptime(self) -> Result<MIRComptimeBody, MIRStagedInstr> {
        self.try_map(|instruction| {
            let kind = match instruction.kind {
                MIRStagedInstrKind::Standard(kind) => MIRComptimeInstrKind::Standard(kind),
                MIRStagedInstrKind::CallerReturn { value } => {
                    MIRComptimeInstrKind::Standard(MIRInstrKind::Return { value })
                }
                MIRStagedInstrKind::Comptime(kind) => MIRComptimeInstrKind::Comptime(kind),
                _ => return Err(instruction),
            };
            Ok(MIRInstr::new(kind, instruction.token_range))
        })
    }
}
