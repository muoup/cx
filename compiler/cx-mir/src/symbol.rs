use cx_ast::ast::template::CXTemplatePrototype;

use crate::mir::{
    data::MIRTypeId,
    expression::{MIRExpression, MIRPureExpression},
};

pub mod resolution;

#[derive(Clone, Debug)]
pub enum MIRSymbol {
    Type(MIRTypeId),
    Value(MIRExpression),
    PureValue(MIRPureExpression),
    Template(CXTemplatePrototype, Box<MIRSymbol>),
}

impl MIRSymbol {
    pub fn as_value(&self) -> Option<MIRExpression> {
        match &self {
            MIRSymbol::Value(value) => Some(value.clone()),
            MIRSymbol::PureValue(value) => Some(value.as_value()),

            _ => None,
        }
    }

    pub fn as_pure(&self) -> Option<MIRPureExpression> {
        match &self {
            MIRSymbol::PureValue(value) => Some(value.clone()),

            _ => None,
        }
    }

    pub fn as_type(&self) -> Option<MIRTypeId> {
        match &self {
            MIRSymbol::Type(id) => Some(id.clone()),

            _ => None
        }
    }
}
