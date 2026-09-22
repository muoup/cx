use std::collections::BTreeMap;

use cx_thir::thir::{
    comptime::THIRStagedParameter,
    expression::{THIRExpression, THIRLocalID},
};
use crate::constant::MIRConstant;

#[derive(Debug, Clone)]
pub struct MIRStagedExpression<'thir> {
    pub expression: &'thir THIRExpression,
    pub parameters: &'thir [THIRStagedParameter],
    pub captures: BTreeMap<THIRLocalID, MIRConstant>,
}
