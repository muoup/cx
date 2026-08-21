use cx_hir::ast::expression::HIRExpression;
use cx_hir::ast::template::HIRTemplateInput;
use cx_log::{CXRawResult, CXResult};
use cx_thir::EnvironmentNamespace;
use cx_thir::symbol::MIRSymbol;
use cx_thir::thir::data::{THIRComptimeFnPrototype, THIRType, THIRTypeID};
use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind, THIRLocalID};
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;

use crate::environment::TypeEnvironment;
use cx_tokens::TokenRange;

/// Richer representation of a typechecking result. Most expressions are ready MIR immediately,
/// but some syntax needs deferred completion, such as template callees that require call-site
/// argument types or expressions whose type must come from context.
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
pub enum TypecheckState {
    Ready(THIRExpression),
    IncompleteTemplatedCallee {
        name: QualifiedName,
        template_input: Option<HIRTemplateInput>,
    },
    NeedsExpectedType(ExpectedTypeDeferredExpr),
}

#[derive(Debug, Clone)]
pub struct StagedFunctionValue {
    pub namespace: EnvironmentNamespace,
    pub params: Vec<(CXIdent, THIRType)>,
    pub body: Box<HIRExpression>,
    pub return_type: THIRType,
}

#[derive(Debug, Clone)]
pub struct ComptimeFunctionValue {
    pub prototype: THIRComptimeFnPrototype,
    pub namespace: EnvironmentNamespace,
    pub body: Box<HIRExpression>,
    pub template_bindings: Vec<(CXIdent, THIRTypeID)>,
}

pub struct IncompleteTemplate {
    pub name: QualifiedName,
    pub template_input: Option<HIRTemplateInput>,
}

type ExpectedTypeResolver =
    dyn FnOnce(&mut TypeEnvironment, &EnvironmentNamespace, &THIRType) -> CXResult<THIRExpression>;

pub struct ExpectedTypeDeferredExpr {
    resolver: Box<ExpectedTypeResolver>,
}

impl ExpectedTypeDeferredExpr {
    pub fn new<F>(resolver: F) -> Self
    where
        F: FnOnce(
                &mut TypeEnvironment,
                &EnvironmentNamespace,
                &THIRType,
            ) -> CXResult<THIRExpression>
            + 'static,
    {
        Self {
            resolver: Box::new(resolver),
        }
    }

    fn resolve(
        self,
        env: &mut TypeEnvironment,
        namespace: &EnvironmentNamespace,
        expected_type: &THIRType,
    ) -> CXResult<THIRExpression> {
        (self.resolver)(env, namespace, expected_type)
    }
}

impl std::fmt::Debug for ExpectedTypeDeferredExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ExpectedTypeDeferredExpr { .. }")
    }
}

pub enum TypecheckExtract<T> {
    Fail(TypecheckResult),
    Succ(T),
}

#[derive(Debug)]
pub struct TypecheckResult {
    /// The accumulated expression
    expression: TypecheckState,
    /// Binding/place information for expressions that still denote a local place.
    binding: Option<TypecheckedBinding>,
    /// True when this value adopts an existing region instead of initializing a fresh one.
    adopting: bool,
}

impl From<THIRExpression> for TypecheckResult {
    fn from(expression: THIRExpression) -> Self {
        Self {
            expression: TypecheckState::Ready(expression),
            binding: None,
            adopting: false,
        }
    }
}

impl TypecheckResult {
    pub fn new(_type: THIRType, kind: THIRExpressionKind) -> Self {
        Self {
            expression: TypecheckState::Ready(THIRExpression {
                token_range: TokenRange::internal(),
                kind,
                _type,
            }),
            binding: None,
            adopting: false,
        }
    }

    pub fn standard_ready_assure(
        self,
        env: &TypeEnvironment,
        token_range: &TokenRange,
    ) -> CXResult<TypecheckResult> {
        match self.expression {
            TypecheckState::Ready(_) => Ok(self),
            TypecheckState::Comptime(_) => env.log_error(
                token_range,
                "Comptime value cannot be used as a runtime expression".to_string(),
            ),
            TypecheckState::IncompleteTemplatedCallee { .. } => env.log_error(
                token_range,
                "Could not deduce templated function parameters".to_string(),
            ),
            TypecheckState::NeedsExpectedType(_) => env.log_error(
                token_range,
                "Could not resolve expression, expected type required but not provided".to_string(),
            ),
        }
    }

    pub fn standard_ready_coerce(
        self,
        env: &TypeEnvironment,
        token_range: &TokenRange,
    ) -> CXResult<THIRExpression> {
        self.standard_ready_assure(env, token_range)
            .map(|t| t.internal_ready_assertion())
    }

    pub fn internal_ready_assertion(self) -> THIRExpression {
        match self.expression {
            TypecheckState::Ready(expr) => expr,

            _ => unreachable!(
                "Expected TypecheckResult to be ready, but was not: {:?}",
                self.expression
            ),
        }
    }

    pub fn incomplete_template(
        name: QualifiedName,
        template_input: Option<HIRTemplateInput>,
    ) -> Self {
        Self {
            expression: TypecheckState::IncompleteTemplatedCallee {
                name,
                template_input,
            },
            binding: None,
            adopting: false,
        }
    }

    pub fn with_binding(mut self, binding: TypecheckedBinding) -> Self {
        self.binding = Some(binding);
        self
    }

    pub fn with_adopting(mut self) -> Self {
        self.adopting = true;
        self
    }

    pub fn binding(&self) -> Option<&TypecheckedBinding> {
        self.binding.as_ref()
    }

    pub fn is_adopting(&self) -> bool {
        self.adopting
    }

    pub fn ready_expression(&self) -> Option<&THIRExpression> {
        match &self.expression {
            TypecheckState::Ready(expression) => Some(expression),
            _ => None,
        }
    }

    pub fn try_into_expression(self) -> TypecheckExtract<THIRExpression> {
        match self.expression {
            TypecheckState::Ready(expression) => TypecheckExtract::Succ(expression),
            expression => TypecheckExtract::Fail(Self { expression, ..self }),
        }
    }

    pub fn try_into_comptime_value(self) -> TypecheckExtract<ComptimeTypecheckValue> {
        match self.expression {
            TypecheckState::Comptime(value) => TypecheckExtract::Succ(value),
            expression => TypecheckExtract::Fail(Self { expression, ..self }),
        }
    }

    pub fn into_incomplete_callee_parts(self) -> Option<IncompleteTemplate> {
        match self.expression {
            TypecheckState::IncompleteTemplatedCallee {
                name,
                template_input,
            } => Some(IncompleteTemplate {
                name,
                template_input,
            }),
            _ => None,
        }
    }

    /// Get the type of this typecheck result's expression
    pub fn ready_type(&self) -> Option<&THIRType> {
        match &self.expression {
            TypecheckState::Ready(expression) => Some(&expression._type),
            TypecheckState::Comptime(ComptimeTypecheckValue::StagedExpr(expression)) => {
                Some(&expression._type)
            }
            TypecheckState::Comptime(ComptimeTypecheckValue::Value(value)) => {
                // FIXME: This allocates a type value; this path is only used for diagnostics and
                // template deduction should avoid relying on it for non-staged comptime values.
                let _ = value;
                None
            }
            TypecheckState::Comptime(ComptimeTypecheckValue::Function(_)) => None,
            TypecheckState::Comptime(ComptimeTypecheckValue::StagedFunction(_)) => None,
            TypecheckState::NeedsExpectedType(_) => None,
            TypecheckState::IncompleteTemplatedCallee { .. } => None,
        }
    }

    pub fn apply_expected_type(
        self,
        env: &mut TypeEnvironment,
        namespace: &EnvironmentNamespace,
        expected_type: &THIRType,
    ) -> CXResult<Self> {
        match self.expression {
            TypecheckState::NeedsExpectedType(expr) => Ok(Self {
                expression: TypecheckState::Ready(expr.resolve(env, namespace, expected_type)?),
                binding: self.binding,
                adopting: self.adopting,
            }),

            _ => Ok(self),
        }
    }

}
