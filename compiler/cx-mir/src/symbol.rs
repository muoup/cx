use cx_ast::{ast::template::CXTemplatePrototype, symbols::CXSymbol};
use cx_log::{CXErrorBase, CXRawResult};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::{
    EnvironmentNamespace,
    mir::{
        data::{MIRFunctionPrototype, MIRTypeId, MIRTypeKind},
        expression::{MIRExpression, MIRExpressionKind},
    },
    type_context::MIRTypeContext,
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

    pub fn as_pattern_target(&self, env: &impl MIRTypeContext) -> Option<QualifiedName> {
        match self {
            MIRSymbol::Type(id) => env.type_id_lookup_identifier(*id).cloned(),

            MIRSymbol::Template {
                name,
                namespace,
                source,
                ..
            } => {
                if source.is_type() {
                    Some(QualifiedName::new(
                        namespace.as_namespace_path().clone(),
                        name.clone(),
                    ))
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    pub fn as_expression(&self) -> CXRawResult<MIRExpression> {
        match self {
            MIRSymbol::FunctionReference(prototype) => Ok(MIRExpression {
                token_range: TokenRange::internal(),
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
                CXErrorBase::raw_result("Could not deduce arguments to template")
            }

            _ => CXErrorBase::raw_result("Symbol does not refer to a value"),
        }
    }
}
