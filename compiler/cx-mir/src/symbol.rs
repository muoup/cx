use cx_ast::{ast::template::CXTemplatePrototype, symbols::CXSymbol};
use cx_util::{identifier::CXIdent};

use crate::{
    EnvironmentNamespace,
    mir::{data::MIRTypeId, expression::MIRExpression},
};

#[derive(Clone, Debug)]
pub enum MIRSymbol {
    Type(MIRTypeId),
    Expression(MIRExpression),
    Template {
        input: CXTemplatePrototype,
        name: CXIdent,
        namespace: EnvironmentNamespace,
        source: Box<CXSymbol>,
    },
}

impl MIRSymbol {
    pub fn as_type_id(&self) -> Option<MIRTypeId> {
        match self {
            MIRSymbol::Type(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_expression(&self) -> Option<MIRExpression> {
        match self {
            MIRSymbol::Expression(value) => Some(value.clone()),

            _ => None,
        }
    }
}
