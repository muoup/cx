use cx_ast::ast::template::CXTemplatePrototype;

use crate::mir::{data::MIRType, expression::{MIRExpression, MIRPureExpression}};

pub mod registry;
pub mod resolution;

#[derive(Clone, Debug)]
pub enum MIRSymbol {
    Type(MIRType),
    Value(MIRExpression),
    PureValue(MIRPureExpression),
    Template(CXTemplatePrototype, Box<MIRSymbol>)
}

impl MIRSymbol {
    fn as_value(&self) -> Option<MIRExpression> {
        match &self {
            MIRSymbol::Value(value) => Some(value.clone()),
            MIRSymbol::PureValue(value) => Some(value.as_value()),

            _ => None,
        }
    }

    fn as_pure(&self) -> Option<MIRPureExpression> {
        match &self {
            MIRSymbol::PureValue(value) => Some(value.clone()),
            
            _ => None,
        }
    }
}