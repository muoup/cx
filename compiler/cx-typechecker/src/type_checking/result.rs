use cx_ast::ast::template::CXTemplateInput;
use cx_mir::EnvironmentNamespace;
use cx_mir::mir::data::MIRType;
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::symbol::MIRSymbol;
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;
use cx_util::{CXError, CXResult};

use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use cx_tokens::TokenRange;

/// Richer representation of a typechecking result. Useful for edge cases where we need to carry implicit behavior
/// not representable by the type system due to move semantics. We want to model CXExpr -> MIRExpr typechecking as
/// immutable after evaluation, so we must contain all mutable state within a meta structure over the typecheck.
///
/// For instance, when evaluating a member function, it is modeled as a free function with an 'implicit parameter'.
/// The implicit parameter is an MIRExpression that could be embedded in the type of a function, however that would
/// require either moving out of said type when constructing the parameter list (breaks mutability rule), cloning the
/// expression (expensive), or having the rules around 'implicit parameters' be handled every time we reason about a
/// method call (leaky).
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
        context: IncompleteCalleeContext,
    },
    NeedsExpectedType(ExpectedTypeDeferredExpr),
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

impl<T> TypecheckExtract<T> {
    pub fn into_result<F>(self, f: F) -> CXResult<T>
    where
        F: FnOnce(TypecheckResult) -> CXResult<T>,
    {
        match self {
            Self::Succ(value) => Ok(value),
            Self::Fail(result) => f(result),
        }
    }
}

pub struct CalleeExtraction {
    pub function: MIRExpression,
    pub implicit_args: Vec<MIRExpression>,
}

impl CalleeExtraction {
    pub fn new(function: MIRExpression, implicit_args: Vec<MIRExpression>) -> Self {
        Self {
            function,
            implicit_args,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PendingReceiver {
    pub source: MIRExpression,
    pub binding: Option<TypecheckedBinding>,
}

impl PendingReceiver {
    pub fn new(source: MIRExpression, binding: Option<TypecheckedBinding>) -> Self {
        Self { source, binding }
    }
}

#[derive(Debug, Clone, Default)]
pub struct IncompleteCalleeContext {
    pub source_base_type: Option<MIRType>,
    pub pending_receiver: Option<PendingReceiver>,
}

impl IncompleteCalleeContext {
    pub fn none() -> Self {
        Self::default()
    }

    pub fn member(source_base_type: MIRType, pending_receiver: PendingReceiver) -> Self {
        Self {
            source_base_type: Some(source_base_type),
            pending_receiver: Some(pending_receiver),
        }
    }
}

#[derive(Debug)]
pub struct TypecheckResult {
    /// The accumulated expression
    expression: TypecheckState,
    /// Implicit parameters carried upward for call sites (e.g. member receivers)
    implicit_parameters: Vec<MIRExpression>,
    /// Binding/place information for expressions that still denote a local place.
    binding: Option<TypecheckedBinding>,
    /// True when this value adopts an existing region instead of initializing a fresh one.
    adopting: bool,
}

impl From<MIRExpression> for TypecheckResult {
    fn from(expression: MIRExpression) -> Self {
        Self {
            expression: TypecheckState::Ready(expression),
            implicit_parameters: Vec::new(),
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
            implicit_parameters: Vec::new(),
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

    pub fn incomplete_templated_callee(
        name: QualifiedName,
        template_input: Option<CXTemplateInput>,
        context: IncompleteCalleeContext,
    ) -> Self {
        Self {
            expression: TypecheckState::IncompleteTemplatedCallee {
                name,
                template_input,
                context,
            },
            implicit_parameters: Vec::new(),
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
            implicit_parameters: Vec::new(),
            binding: None,
            adopting: false,
        }
    }

    pub fn with_implicit_parameters(mut self, implicit_parameters: Vec<MIRExpression>) -> Self {
        self.implicit_parameters = implicit_parameters;
        self
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

    pub fn try_into_callee(self) -> TypecheckExtract<CalleeExtraction> {
        match self.expression {
            TypecheckState::Ready(function) => {
                TypecheckExtract::Succ(CalleeExtraction::new(function, self.implicit_parameters))
            }
            expression => TypecheckExtract::Fail(Self { expression, ..self }),
        }
    }

    pub fn into_incomplete_callee_parts(
        self,
    ) -> Option<(
        QualifiedName,
        Option<CXTemplateInput>,
        Option<MIRType>,
        Vec<MIRExpression>,
        Option<PendingReceiver>,
    )> {
        match self.expression {
            TypecheckState::IncompleteTemplatedCallee {
                name,
                template_input,
                context,
            } => Some((
                name,
                template_input,
                context.source_base_type,
                self.implicit_parameters,
                context.pending_receiver,
            )),
            _ => None,
        }
    }

    /// Get the type of this typecheck result's expression
    pub fn get_type_if_ready(&self) -> CXResult<Option<MIRType>> {
        match &self.expression {
            TypecheckState::Ready(expression) => Ok(Some(expression._type.clone())),
            TypecheckState::NeedsExpectedType(_) => Ok(None),
            TypecheckState::IncompleteTemplatedCallee { name, .. } => {
                CXError::create_result(format!(
                    "Templated function '{}' requires an argument list for template deduction",
                    name
                ))
            }
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
                implicit_parameters: self.implicit_parameters,
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
        context: IncompleteCalleeContext,
    ) -> CXResult<Self> {
        if matches!(symbol, MIRSymbol::Template { .. }) {
            return Ok(Self::incomplete_templated_callee(
                name,
                template_input,
                context,
            ));
        }

        symbol.as_expression().map(Self::from)
    }
}
