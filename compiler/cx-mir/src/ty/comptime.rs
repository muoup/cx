use crate::ty::MIRTypeID;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRComptimeType {
    Standard(MIRTypeID),
    StagedExpression(MIRTypeID)
}