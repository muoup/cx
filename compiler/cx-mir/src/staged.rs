use std::sync::Arc;

use crate::{MIRBody, MIRPlaceID, MIRRegister, MIRTypeID};

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
