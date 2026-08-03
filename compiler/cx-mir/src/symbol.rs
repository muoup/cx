use cx_ast::{
    ast::{expression::CXExpression, template::CXTemplatePrototype},
    symbols::CXSymbol,
};
use cx_log::error::{CXRawResult, message::CXStdErrMessage};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::{
    EnvironmentNamespace,
    mir::{
        data::{
            MIRComptimeFunctionPrototype, MIRFunctionPrototype, MIRType, MIRTypeId, MIRTypeKind,
        },
        expression::{MIRExpression, MIRExpressionKind},
    },
    type_context::MIRTypeContext,
};

#[derive(Clone, Debug)]
pub enum MIRSymbol {
    Type(MIRTypeId),
    FunctionReference(MIRFunctionPrototype),
    ComptimeFunctionReference {
        prototype: MIRComptimeFunctionPrototype,
        namespace: EnvironmentNamespace,
        body: Box<CXExpression>,
        template_bindings: Vec<(CXIdent, MIRTypeId)>,
    },
    StagedExpression {
        id: u64,
        namespace: EnvironmentNamespace,
        expr: Box<CXExpression>,
        expected_type: MIRType,
    },
    StagedExpressionFunction {
        namespace: EnvironmentNamespace,
        params: Vec<(CXIdent, MIRType)>,
        body: Box<CXExpression>,
        return_type: MIRType,
    },
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
            MIRSymbol::FunctionReference(prototype) => CXRawResult::Ok(MIRExpression {
                token_range: TokenRange::internal(),
                _type: MIRTypeKind::Function {
                    signature: Box::new(prototype.signature().clone()),
                }
                .into(),
                kind: MIRExpressionKind::FunctionReference {
                    name: CXIdent::new(prototype.name()),
                },
            }),

            MIRSymbol::Expression(expr) => CXRawResult::Ok(expr.clone()),

            MIRSymbol::ComptimeFunctionReference { .. } => CXStdErrMessage::result(
                "TYPE ERROR",
                "Comptime function cannot be used in runtime contexts",
            ),

            MIRSymbol::StagedExpression { .. } | MIRSymbol::StagedExpressionFunction { .. } => {
                CXStdErrMessage::result(
                    "TYPE ERROR",
                    "Staged expression cannot be used in runtime contexts",
                )
            }

            MIRSymbol::Template { .. } => {
                CXStdErrMessage::result("TYPE ERROR", "Could not deduce arguments to template")
            }

            _ => CXStdErrMessage::result("TYPE ERROR", "Symbol does not refer to a value"),
        }
    }
}
