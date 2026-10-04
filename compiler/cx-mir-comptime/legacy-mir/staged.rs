use std::collections::BTreeMap;

use crate::value::MIRComptimeValue;
use cx_thir::thir::{
    comptime::THIRStagedParameter,
    expression::{THIRExpression, THIRLocalID},
};

#[derive(Debug, Clone)]
pub struct MIRStagedExpression<'thir> {
    pub expression: &'thir THIRExpression,
    pub parameters: &'thir [THIRStagedParameter],
    pub captures: BTreeMap<THIRLocalID, MIRComptimeValue>,
}
