use crate::ty::{MIRType, MIRTypeID};

pub struct MIRComptimeType {
    _type: MIRTypeID,
    staged_metadata: MIRStagedMetadata,
}

impl MIRComptimeType {
    pub fn new(_type: MIRTypeID, staged_metadata: MIRStagedMetadata) -> Self {
        Self {
            _type,
            staged_metadata,
        }
    }

    pub fn _type(&self) -> MIRTypeID {
        self._type
    }

    pub fn staged_metadata(&self) -> &MIRStagedMetadata {
        &self.staged_metadata
    }
}

pub struct MIRStagedMetadata {
    escaping_return_bound: Option<MIRTypeID>,
    escaping_yield_bound: Option<MIRTypeID>
}

impl MIRStagedMetadata {
    pub fn new() -> Self {
        Self {
            escaping_return_bound: None,
            escaping_yield_bound: None
        }
    }

    pub fn define_escaping_return(self, bound: MIRTypeID) -> Self {
        self.escaping_return_bound = Some(bound);
        self
    }

    pub fn define_escaping_yield(self, bound: MIRTypeID) -> Self {
        self.escaping_yield_bound = Some(bound);
        self
    }
}
