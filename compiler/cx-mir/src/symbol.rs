use cx_ast::{ast::template::CXTemplatePrototype, symbols::CXSymbol};
use cx_util::{CXError, CXResult, identifier::CXIdent};

use crate::{
    EnvironmentNamespace,
    mir::{
        data::{MIRFunctionPrototype, MIRTypeId, MIRTypeKind},
        expression::{MIRExpression, MIRExpressionKind},
    },
};

#[derive(Clone, Debug)]
pub enum MIRSymbol {
    Type(MIRTypeId),
    FunctionReference(MIRFunctionPrototype),
    Expression(MIRExpression),
    Template {
        template_prototype: CXTemplatePrototype,
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

    pub fn as_expression(&self) -> CXResult<MIRExpression> {
        match self {
            MIRSymbol::FunctionReference(prototype) => Ok(MIRExpression {
                token_range: None,
                _type: MIRTypeKind::Function {
                    signature: Box::new(prototype.signature().clone()),
                }
                .into(),
                kind: MIRExpressionKind::FunctionReference {
                    name: CXIdent::new(prototype.name()),
                },
            }),

            MIRSymbol::Expression(expr) => Ok(expr.clone()),

            MIRSymbol::Template { .. } => {
                CXError::create_result("Could not deduce arguments to template")
            }

            _ => CXError::create_result("Symbol does not refer to a value"),
        }
    }
}
