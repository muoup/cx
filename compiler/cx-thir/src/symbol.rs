use cx_hir::{
    ast::{expression::HIRExpression, template::HIRTemplatePrototype, types::HIRTagKind},
    symbols::HIRSymbol,
};
use cx_log::error::{CXRawResult, message::CXStdErrMessage};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::{
    EnvironmentNamespace,
    thir::{
        data::{THIRComptimeFnPrototype, THIRFnPrototype, THIRType, THIRTypeID, THIRTypeKind},
        expression::{THIRExpression, THIRExpressionKind, THIRLocalID},
    },
    type_context::THIRTypeContext,
};

#[derive(Clone, Debug)]
pub enum MIRSymbol {
    Type(THIRTypeID),
    FunctionReference(THIRFnPrototype),
    ComptimeFunctionReference {
        prototype: THIRComptimeFnPrototype,
        namespace: EnvironmentNamespace,
    },
    StagedExpression {
        id: u64,
        namespace: EnvironmentNamespace,
        expr: Box<HIRExpression>,
        expected_type: THIRType,
    },
    /// A local binding holding a parameterized staged value, e.g. a staged
    /// parameter of a comptime function. The referenced value is supplied by
    /// the caller at evaluation time; no body is stored here.
    StagedExpressionFunction {
        local_id: THIRLocalID,
        params: Vec<THIRType>,
        return_type: THIRType,
    },
    Expression(THIRExpression),
    Template {
        template_prototype: HIRTemplatePrototype,
        name: CXIdent,
        namespace: EnvironmentNamespace,
        source: Box<HIRSymbol>,
        tag: Option<HIRTagKind>,
    },
}

impl MIRSymbol {
    pub fn as_type_id(&self) -> Option<THIRTypeID> {
        match self {
            MIRSymbol::Type(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_pattern_target(&self, env: &impl THIRTypeContext) -> Option<QualifiedName> {
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

    pub fn as_expression(&self) -> CXRawResult<THIRExpression> {
        match self {
            MIRSymbol::FunctionReference(prototype) => CXRawResult::Ok(THIRExpression {
                token_range: TokenRange::internal(),
                _type: THIRTypeKind::Function {
                    signature: Box::new(prototype.signature().clone()),
                }
                .into(),
                kind: THIRExpressionKind::FunctionReference {
                    name: CXIdent::new(prototype.symbol_name()),
                    debug_name: prototype.debug_name().cloned(),
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
