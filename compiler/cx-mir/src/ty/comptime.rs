use crate::ty::MIRTypeID;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRComptimeType {
    Standard(MIRTypeID),
    StagedExpression {
        result: MIRTypeID,
        params: Vec<MIRTypeID>,
    },
}

impl MIRComptimeType {
    pub fn result_type(&self) -> MIRTypeID {
        match self {
            Self::Standard(ty) | Self::StagedExpression { result: ty, .. } => *ty,
        }
    }
}
