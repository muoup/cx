use std::fmt::{Debug, Formatter};

use cx_hir::ast::{expression::HIRExpression, template::HIRTemplateInput};
use cx_log::{CXRawResult, CXResult};
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_thir::{
    symbol::MIRSymbol,
    thir::{
        comptime::THIRStagedExpr,
        data::{THIRComptimeFnPrototype, THIRComptimeValueType, THIRType},
        expression::{THIRExpression, THIRExpressionKind, THIRLocalID},
    },
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::environment::TypeEnvironment;

#[derive(Debug, Clone)]
pub struct TypecheckedBinding {
    pub root: CXIdent,
    pub local_id: THIRLocalID,
    pub kind: BindingPlaceKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingPlaceKind {
    Local,
    Projection,
}

impl TypecheckedBinding {
    pub fn local(root: CXIdent, local_id: THIRLocalID) -> Self {
        Self {
            root,
            local_id,
            kind: BindingPlaceKind::Local,
        }
    }

    pub fn projection(root: CXIdent, local_id: THIRLocalID) -> Self {
        Self {
            root,
            local_id,
            kind: BindingPlaceKind::Projection,
        }
    }

    pub fn project(&self) -> Self {
        Self::projection(self.root.clone(), self.local_id)
    }
}

#[derive(Debug)]
pub struct StandardTC {
    expression: THIRExpression,
    binding: Option<TypecheckedBinding>,
    adopting: bool,
}

impl StandardTC {
    pub(crate) fn into_expression(self) -> THIRExpression {
        self.expression
    }
}

#[derive(Debug, Clone)]
pub struct ComptimeFunctionTC {
    pub prototype: THIRComptimeFnPrototype,
}

#[derive(Debug, Clone)]
pub struct StagedBindingTC {
    pub reference: THIRExpression,
    pub params: Vec<THIRType>,
    pub return_type: THIRType,
}

#[derive(Debug, Clone)]
pub enum StagedTC {
    Literal(THIRStagedExpr),
    Binding(StagedBindingTC),
}

#[derive(Debug)]
pub enum TypecheckedExpr {
    Standard(StandardTC),
    Staged(StagedTC),
    ComptimeFunction(ComptimeFunctionTC),
}

#[derive(Debug)]
pub struct IncompleteTemplate {
    pub name: QualifiedName,
    pub template_input: Option<HIRTemplateInput>,
}

#[derive(Debug)]
pub struct DeferredStagedExpr {
    pub params: Vec<CXIdent>,
    pub body: Box<HIRExpression>,
}

type ExpectedTypeResolver<T> =
    dyn FnOnce(&mut TypeEnvironment, &NamespacePath, &THIRType) -> CXResult<T>;

pub enum TypecheckResult<T = TypecheckedExpr> {
    Ready(T),
    IncompleteTemplate(IncompleteTemplate),
    NeedsExpectedType(Box<ExpectedTypeResolver<T>>),
    NeedsStagedType(DeferredStagedExpr),
}

impl<T: Debug> Debug for TypecheckResult<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ready(value) => f.debug_tuple("Ready").field(value).finish(),
            Self::IncompleteTemplate(value) => {
                f.debug_tuple("IncompleteTemplate").field(value).finish()
            }
            Self::NeedsExpectedType(_) => f.write_str("NeedsExpectedType { .. }"),
            Self::NeedsStagedType(value) => f.debug_tuple("NeedsStagedType").field(value).finish(),
        }
    }
}

impl From<THIRExpression> for TypecheckResult {
    fn from(expression: THIRExpression) -> Self {
        Self::standard(expression)
    }
}

impl TypecheckResult {
    pub fn new(_type: THIRType, kind: THIRExpressionKind) -> Self {
        Self::standard(THIRExpression {
            token_range: TokenRange::internal(),
            kind,
            _type,
        })
    }

    pub fn standard(expression: THIRExpression) -> Self {
        Self::Ready(TypecheckedExpr::Standard(StandardTC {
            expression,
            binding: None,
            adopting: false,
        }))
    }

    pub fn staged_literal(value: THIRStagedExpr) -> Self {
        Self::Ready(TypecheckedExpr::Staged(StagedTC::Literal(value)))
    }

    pub fn staged_binding(value: StagedBindingTC) -> Self {
        Self::Ready(TypecheckedExpr::Staged(StagedTC::Binding(value)))
    }

    pub fn comptime_function(prototype: THIRComptimeFnPrototype) -> Self {
        Self::Ready(TypecheckedExpr::ComptimeFunction(ComptimeFunctionTC {
            prototype,
        }))
    }

    pub fn incomplete_template(
        name: QualifiedName,
        template_input: Option<HIRTemplateInput>,
    ) -> Self {
        Self::IncompleteTemplate(IncompleteTemplate {
            name,
            template_input,
        })
    }

    pub fn needs_expected_type<F>(resolver: F) -> Self
    where
        F: FnOnce(&mut TypeEnvironment, &NamespacePath, &THIRType) -> CXResult<THIRExpression>
            + 'static,
    {
        Self::NeedsExpectedType(Box::new(move |env, namespace, expected_type| {
            resolver(env, namespace, expected_type).map(|expression| {
                TypecheckedExpr::Standard(StandardTC {
                    expression,
                    binding: None,
                    adopting: false,
                })
            })
        }))
    }

    pub fn needs_staged_type(params: Vec<CXIdent>, body: Box<HIRExpression>) -> Self {
        Self::NeedsStagedType(DeferredStagedExpr { params, body })
    }

    pub fn standard_ready_assure(
        self,
        env: &TypeEnvironment,
        token_range: &TokenRange,
    ) -> CXResult<Self> {
        match self {
            Self::Ready(TypecheckedExpr::Standard(_)) => Ok(self),
            Self::Ready(TypecheckedExpr::Staged(_)) => env.log_error(
                token_range,
                "Staged expression cannot be used as a runtime expression".to_string(),
            ),
            Self::Ready(TypecheckedExpr::ComptimeFunction(_)) => env.log_error(
                token_range,
                "Comptime function cannot be used as a value".to_string(),
            ),
            Self::IncompleteTemplate(_) => env.log_error(
                token_range,
                "Could not deduce templated function parameters".to_string(),
            ),
            Self::NeedsExpectedType(_) => env.log_error(
                token_range,
                "Could not resolve expression, expected type required but not provided".to_string(),
            ),
            Self::NeedsStagedType(_) => env.log_error(
                token_range,
                "Could not resolve staged expression, staged parameter types required but not provided"
                    .to_string(),
            ),
        }
    }

    pub fn standard_ready_coerce(
        self,
        env: &TypeEnvironment,
        token_range: &TokenRange,
    ) -> CXResult<THIRExpression> {
        self.standard_ready_assure(env, token_range)
            .map(Self::internal_ready_assertion)
    }

    pub fn internal_ready_assertion(self) -> THIRExpression {
        match self {
            Self::Ready(TypecheckedExpr::Standard(value)) => value.into_expression(),
            value => unreachable!("Expected a ready standard expression, found {value:?}"),
        }
    }

    pub fn with_binding(mut self, binding: TypecheckedBinding) -> Self {
        if let Self::Ready(TypecheckedExpr::Standard(value)) = &mut self {
            value.binding = Some(binding);
        }
        self
    }

    pub fn with_adopting(mut self) -> Self {
        if let Self::Ready(TypecheckedExpr::Standard(value)) = &mut self {
            value.adopting = true;
        }
        self
    }

    pub fn binding(&self) -> Option<&TypecheckedBinding> {
        match self {
            Self::Ready(TypecheckedExpr::Standard(value)) => value.binding.as_ref(),
            _ => None,
        }
    }

    pub fn is_adopting(&self) -> bool {
        match self {
            Self::Ready(TypecheckedExpr::Standard(value)) => value.adopting,
            _ => false,
        }
    }

    pub fn ready_expression(&self) -> Option<&THIRExpression> {
        match self {
            Self::Ready(TypecheckedExpr::Standard(value)) => Some(&value.expression),
            _ => None,
        }
    }

    pub fn set_token_range_if_missing(&mut self, token_range: TokenRange) -> CXResult<()> {
        let Some(expression) = self.ready_standard_mut() else {
            return Ok(());
        };

        if !matches!(expression.token_range, TokenRange::Source { .. }) {
            expression.token_range = token_range;
        }

        Ok(())
    }

    pub fn ready_type(&self) -> Option<&THIRType> {
        self.ready_expression().map(|expression| &expression._type)
    }

    pub fn apply_expected_type(
        self,
        env: &mut TypeEnvironment,
        namespace: &NamespacePath,
        expected_type: &THIRType,
    ) -> CXResult<Self> {
        match self {
            Self::NeedsExpectedType(resolver) => {
                resolver(env, namespace, expected_type).map(Self::Ready)
            }
            _ => Ok(self),
        }
    }

    pub fn apply_staged_type(
        self,
        env: &mut TypeEnvironment,
        namespace: &NamespacePath,
        value_type: &THIRComptimeValueType,
    ) -> CXResult<Self> {
        match self {
            Self::NeedsStagedType(deferred) => {
                crate::type_checking::staged_expr::complete_staged_expr(
                    env, namespace, deferred, value_type,
                )
                .map(Self::staged_literal)
            }
            _ => Ok(self),
        }
    }

    pub fn from_symbol(
        symbol: MIRSymbol,
        name: QualifiedName,
        template_input: Option<HIRTemplateInput>,
    ) -> CXRawResult<Self> {
        match symbol {
            MIRSymbol::Template { .. } => Ok(Self::incomplete_template(name, template_input)),
            MIRSymbol::ComptimeFunctionReference { prototype, .. } => {
                Ok(Self::comptime_function(prototype))
            }
            _ => symbol.as_expression().map(Self::from),
        }
    }

    fn ready_standard_mut(&mut self) -> Option<&mut THIRExpression> {
        match self {
            Self::Ready(TypecheckedExpr::Standard(value)) => Some(&mut value.expression),
            _ => None,
        }
    }
}
