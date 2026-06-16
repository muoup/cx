use cx_ast::ast::template::CXTemplateInput;
use cx_log::CXResult;
use cx_mir::EnvironmentNamespace;
use cx_mir::mir::data::MIRType;
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::symbol::MIRSymbol;
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;

use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use cx_tokens::TokenRange;

/// Richer representation of a typechecking result. Most expressions are ready MIR immediately,
/// but some syntax needs deferred completion, such as template callees that require call-site
/// argument types or expressions whose type must come from context.
#[derive(Debug, Clone)]
pub struct TypecheckedBinding {
    pub root: CXIdent,
    pub kind: BindingPlaceKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingPlaceKind {
    Local,
    Projection,
}

impl TypecheckedBinding {
    pub fn local(root: CXIdent) -> Self {
        Self {
            root,
            kind: BindingPlaceKind::Local,
        }
    }

    pub fn projection(root: CXIdent) -> Self {
        Self {
            root,
            kind: BindingPlaceKind::Projection,
        }
    }

    pub fn project(&self) -> Self {
        Self::projection(self.root.clone())
    }
}

#[derive(Debug)]
pub enum TypecheckState {
    Ready(MIRExpression),
    IncompleteTemplatedCallee {
        name: QualifiedName,
        template_input: Option<CXTemplateInput>,
    },
    NeedsExpectedType(ExpectedTypeDeferredExpr),
}

pub struct IncompleteTemplate {
    pub name: QualifiedName,
    pub template_input: Option<CXTemplateInput>,
}

type ExpectedTypeResolver =
    dyn FnOnce(&mut TypeEnvironment, &EnvironmentNamespace, &MIRType) -> CXResult<MIRExpression>;

pub struct ExpectedTypeDeferredExpr {
    resolver: Box<ExpectedTypeResolver>,
}

impl ExpectedTypeDeferredExpr {
    pub fn new<F>(resolver: F) -> Self
    where
        F: FnOnce(&mut TypeEnvironment, &EnvironmentNamespace, &MIRType) -> CXResult<MIRExpression>
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
        expected_type: &MIRType,
    ) -> CXResult<MIRExpression> {
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

impl From<MIRExpression> for TypecheckResult {
    fn from(expression: MIRExpression) -> Self {
        Self {
            expression: TypecheckState::Ready(expression),
            binding: None,
            adopting: false,
        }
    }
}

impl TypecheckResult {
    pub fn new(_type: MIRType, kind: MIRExpressionKind) -> Self {
        Self {
            expression: TypecheckState::Ready(MIRExpression {
                token_range: None,
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
            TypecheckState::IncompleteTemplatedCallee { .. } => log_typecheck_error!(
                env,
                token_range,
                "Could not deduce templated function parameters",
            ),
            TypecheckState::NeedsExpectedType(_) => log_typecheck_error!(
                env,
                token_range,
                "Could not resolve expression, expected type required but not provided",
            ),
        }
    }

    pub fn standard_ready_coerce(
        self,
        env: &TypeEnvironment,
        token_range: &TokenRange,
    ) -> CXResult<MIRExpression> {
        self.standard_ready_assure(env, token_range)
            .map(|t| t.internal_ready_assertion())
    }

    pub fn internal_ready_assertion(self) -> MIRExpression {
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
        template_input: Option<CXTemplateInput>,
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

    pub fn needs_expected_type<F>(resolver: F) -> Self
    where
        F: FnOnce(&mut TypeEnvironment, &EnvironmentNamespace, &MIRType) -> CXResult<MIRExpression>
            + 'static,
    {
        Self {
            expression: TypecheckState::NeedsExpectedType(ExpectedTypeDeferredExpr::new(resolver)),
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

    pub fn ready_expression(&self) -> Option<&MIRExpression> {
        match &self.expression {
            TypecheckState::Ready(expression) => Some(expression),
            _ => None,
        }
    }

    pub fn try_into_expression(self) -> TypecheckExtract<MIRExpression> {
        match self.expression {
            TypecheckState::Ready(expression) => TypecheckExtract::Succ(expression),
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
    pub fn ready_type(&self) -> Option<&MIRType> {
        match &self.expression {
            TypecheckState::Ready(expression) => Some(&expression._type),
            TypecheckState::NeedsExpectedType(_) => None,
            TypecheckState::IncompleteTemplatedCallee { .. } => None,
        }
    }

    pub fn set_token_range_if_missing(&mut self, token_range: TokenRange) -> CXResult<()> {
        let expression = match &mut self.expression {
            TypecheckState::Ready(expression) => expression,
            TypecheckState::IncompleteTemplatedCallee { .. }
            | TypecheckState::NeedsExpectedType(_) => return Ok(()),
        };

        if expression.token_range.is_none() {
            expression.token_range = Some(token_range);
        }

        Ok(())
    }

    pub fn apply_expected_type(
        self,
        env: &mut TypeEnvironment,
        namespace: &EnvironmentNamespace,
        expected_type: &MIRType,
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

    pub fn from_symbol(
        symbol: MIRSymbol,
        name: QualifiedName,
        template_input: Option<CXTemplateInput>,
    ) -> CXResult<Self> {
        if matches!(symbol, MIRSymbol::Template { .. }) {
            return Ok(Self::incomplete_template(name, template_input));
        }

        symbol.as_expression().map(Self::from)
    }
}
