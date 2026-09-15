use std::sync::Arc;

use crate::{MIRBasicBlockID, MIRBody, MIRInstr, MIRPlaceID, MIRRegister, MIRStagedExitKind, MIRStagedTargets, MIRTypeID, MIRValue};

#[derive(Debug, Clone, Copy)]
pub enum MIRStagedCapture {
    Register(MIRRegister),
    Place(MIRPlaceID),
}

#[derive(Debug, Clone)]
pub struct MIRStagedTemplate {
    body: MIRBody,
    captures: Arc<[MIRStagedCapture]>,
    params: Arc<[MIRRegister]>,
    result_type: MIRTypeID,
    diverges: bool,
}

impl MIRStagedTemplate {
    pub fn new(
        body: MIRBody,
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

    pub fn body(&self) -> &MIRBody {
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

#[derive(Debug, Clone)]
pub struct MIRStagedBasicBlock {
    id: MIRBasicBlockID,
    instrs: Vec<MIRStagedInstr>,
}

#[derive(Debug, Clone)]
pub enum MIRStagedInstr {
    Standard(MIRInstr),
    
    Return {
        value: MIRValue,
    },
    StagedMove {
        out: MIRRegister,
        value: MIRValue,
    },
    StagedScopeExit {
        kind: MIRStagedExitKind,
    },
    StagedYield {
        value: Option<MIRValue>,
        ty: Option<MIRTypeID>,
    },
    StagedUse {
        value: MIRValue,
        targets: MIRStagedTargets,
    },
}