use std::sync::Arc;

use crate::{MIRBody, MIRRegister, MIRTypeID};

#[derive(Debug, Clone)]
pub struct MIRStagedTemplate {
    body: MIRBody,
    captures: Arc<[MIRRegister]>,
    params: Arc<[MIRRegister]>,
    result_type: MIRTypeID,
    diverges: bool,
}

impl MIRStagedTemplate {
    pub fn new(
        body: MIRBody,
        captures: Vec<MIRRegister>,
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

    pub fn captures(&self) -> &[MIRRegister] {
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
