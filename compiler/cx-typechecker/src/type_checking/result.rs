use cx_ast::ast::expression::CXExpression;
use cx_ast::ast::template::CXTemplateInput;
use cx_log::{CXRawResult, CXResult};
use cx_mir::EnvironmentNamespace;
use cx_mir::mir::data::{MIRComptimeFunctionPrototype, MIRType, MIRTypeId};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::symbol::MIRSymbol;
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;

use crate::comptime::value::ComptimeValue;
use crate::environment::TypeEnvironment;
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
    Comptime(ComptimeTypecheckValue),
    IncompleteTemplatedCallee {
        name: QualifiedName,
        template_input: Option<CXTemplateInput>,
    },
    NeedsExpectedType(ExpectedTypeDeferredExpr),
}

#[derive(Debug, Clone)]
pub enum ComptimeTypecheckValue {
    Function(ComptimeFunctionValue),
    Value(ComptimeValue),
    #[allow(dead_code)]
    StagedExpr(MIRExpression),
}

#[derive(Debug, Clone)]
pub struct ComptimeFunctionValue {
    pub prototype: MIRComptimeFunctionPrototype,
    pub namespace: EnvironmentNamespace,
    pub body: Box<CXExpression>,
    pub template_bindings: Vec<(CXIdent, MIRTypeId)>,
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

    pub fn comptime_function(value: ComptimeFunctionValue) -> Self {
        Self {
            expression: TypecheckState::Comptime(ComptimeTypecheckValue::Function(value)),
            binding: None,
            adopting: false,
        }
    }

    pub fn comptime_value(value: ComptimeValue) -> Self {
        Self {
            expression: TypecheckState::Comptime(ComptimeTypecheckValue::Value(value)),
            binding: None,
            adopting: false,
        }
    }

    #[allow(dead_code)]
    pub fn staged_expr(expression: MIRExpression) -> Self {
        Self {
            expression: TypecheckState::Comptime(ComptimeTypecheckValue::StagedExpr(expression)),
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
    pub fn ready_type(&self) -> Option<&MIRType> {
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
            TypecheckState::NeedsExpectedType(_) => None,
            TypecheckState::IncompleteTemplatedCallee { .. } => None,
        }
    }

    pub fn set_token_range_if_missing(&mut self, token_range: TokenRange) -> CXResult<()> {
        let expression = match &mut self.expression {
            TypecheckState::Ready(expression) => expression,
            TypecheckState::IncompleteTemplatedCallee { .. }
            | TypecheckState::NeedsExpectedType(_)
            | TypecheckState::Comptime(_) => return Ok(()),
        };

        if !matches!(expression.token_range, TokenRange::Source { .. }) {
            expression.token_range = token_range;
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
    ) -> CXRawResult<Self> {
        if matches!(symbol, MIRSymbol::Template { .. }) {
            return Ok(Self::incomplete_template(name, template_input));
        }

        if let MIRSymbol::ComptimeFunctionReference {
            prototype,
            namespace,
            body,
            template_bindings,
        } = symbol
        {
            return Ok(Self::comptime_function(ComptimeFunctionValue {
                prototype,
                namespace,
                body,
                template_bindings,
            }));
        }

        symbol.as_expression().map(Self::from)
    }
}
