use crate::ty::MIRTypeID;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct MIRStagedMetadata {
    escaping_return_bound: Option<MIRTypeID>,
    escaping_yield_bound: Option<MIRTypeID>,
}

impl MIRStagedMetadata {
    pub fn new() -> Self {
        Self {
            escaping_return_bound: None,
            escaping_yield_bound: None,
        }
    }

    pub fn define_escaping_return(mut self, bound: MIRTypeID) -> Self {
        self.escaping_return_bound = Some(bound);
        self
    }

    pub fn define_escaping_yield(mut self, bound: MIRTypeID) -> Self {
        self.escaping_yield_bound = Some(bound);
        self
    }
}
