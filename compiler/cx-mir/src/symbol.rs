use cx_ast::{ast::{expression::CXExpression, template::CXTemplatePrototype}, symbols::CXSymbol};

use crate::{
    mir::{
        data::{MIRFunctionPrototype, MIRTypeId},
        expression::{MIRExpression, MIRPureExpression},
    },
    program::EnvironmentNamespace,
};

pub mod completion;
pub mod mangling;
pub mod resolution;

#[derive(Clone, Debug)]
pub enum MIRSymbol {
    Type(MIRTypeId),
    Value { 
        expr: CXExpression,
        is_constexpr: bool,
    },
    Template {
        input: CXTemplatePrototype,
        name: cx_util::namespace::QualifiedName,
        source: Box<CXSymbol>,
        namespace: EnvironmentNamespace,
    },
}

impl MIRSymbol {
    pub fn as_type_id(&self) -> Option<MIRTypeId> {
        match self {
            MIRSymbol::Type(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_pure_value(&self) -> Option<MIRPureExpression> {
        match self {
            MIRSymbol::PureValue(value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn as_function_prototype(&self) -> Option<&MIRFunctionPrototype> {
        match self {
            MIRSymbol::PureValue(MIRPureExpression::FunctionReference(prototype)) => {
                Some(prototype.as_ref())
            }
            _ => None,
        }
    }

    pub fn into_value(&self) -> Option<MIRExpression> {
        match self {
            MIRSymbol::Value(value) => Some(value.clone()),
            MIRSymbol::PureValue(value) => Some(value.as_value()),
            _ => None,
        }
    }

    pub fn into_pure(&self) -> Option<&MIRPureExpression> {
        match self {
            MIRSymbol::PureValue(value) => Some(value),
            _ => None,
        }
    }

    pub fn into_type_id(&self) -> Option<MIRTypeId> {
        match self {
            MIRSymbol::Type(id) => Some(*id),
            _ => None,
        }
    }

    pub fn into_function_prototype(self) -> Option<MIRFunctionPrototype> {
        match self {
            MIRSymbol::PureValue(MIRPureExpression::FunctionReference(prototype)) => {
                Some(*prototype)
            }
            _ => None,
        }
    }
}
