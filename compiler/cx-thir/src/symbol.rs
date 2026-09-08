use cx_hir::{
    ast::{template::HIRTemplatePrototype, types::HIRTagKind},
    symbols::HIRSymbol,
};
use cx_log::{CXRawResult, catalogue::typecheck};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    NamespacePath,
    thir::{
        data::{
            THIRComptimeFnPrototype, THIRFnPrototype, THIRTemplateInput, THIRType, THIRTypeID,
            THIRTypeKind,
        },
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
        input: THIRTemplateInput,
    },
    StagedExpressionFunction {
        local_id: THIRLocalID,
        params: Vec<THIRType>,
        return_type: THIRType,
    },
    Expression(THIRExpression),
    Template {
        template_prototype: HIRTemplatePrototype,
        name: CXIdent,
        namespace: NamespacePath,
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
                    Some(QualifiedName::new(namespace.clone(), name.clone()))
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    pub fn as_expression(&self) -> CXRawResult<THIRExpression> {
        match self {
            MIRSymbol::FunctionReference(prototype) => Ok(THIRExpression {
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

            // FIXME: We should be able to generate function calls to comptime functions in a runtime function's THIR
            MIRSymbol::ComptimeFunctionReference { .. } => {
                crate::log::log_error(&typecheck::COMPTIME_FUNCTION_RUNTIME_CONTEXT, ())
            }

            // FIXME: Ditto above
            MIRSymbol::StagedExpressionFunction { .. } => {
                crate::log::log_error(&typecheck::STAGED_EXPRESSION_RUNTIME_CONTEXT, ())
            }

            MIRSymbol::Template { .. } => crate::log::log_error(&typecheck::TEMPLATE_DEDUCTION, ()),

            _ => crate::log::log_error(&typecheck::SYMBOL_NOT_VALUE, ()),
        }
    }
}
