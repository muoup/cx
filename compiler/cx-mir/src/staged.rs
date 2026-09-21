use std::collections::BTreeMap;

use cx_thir::thir::{
    comptime::THIRStagedParameter,
    expression::{THIRExpression, THIRLocalID},
};
use cx_util::dense_id;

use crate::constant::MIRConstantID;

dense_id!(MIRStagedID, "staged.");

#[derive(Debug, Clone)]
pub struct MIRStagedExpression<'thir> {
    pub expression: &'thir THIRExpression,
    pub parameters: &'thir [THIRStagedParameter],
    pub captures: BTreeMap<THIRLocalID, MIRConstantID>,
}
