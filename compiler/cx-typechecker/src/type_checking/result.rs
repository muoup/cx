use cx_ast::data::CXTemplateInput;
use cx_mir::mir::data::MIRType;
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::mir::program::MIRBaseMappings;
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;
use cx_util::{CXError, CXResult};

use crate::environment::TypeEnvironment;
use crate::type_checking::value::locals::ensure_binding_available;
use cx_tokens::TokenRange;

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

/// Richer representation of a typechecking result. Useful for edge cases where we need to carry implicit behavior
/// not representable by the type system due to move semantics. We want to model CXExpr -> MIRExpr typechecking as
/// immutable after evaluation, so we must contain all mutable state within a meta structure over the typecheck.
///
/// For instance, when evaluating a member function, it is modeled as a free function with an 'implicit parameter'.
/// The implicit parameter is an MIRExpression that could be embedded in the type of a function, however that would
/// require either moving out of said type when constructing the parameter list (breaks mutability rule), cloning the
/// expression (expensive), or having the rules around 'implicit parameters' be handled every time we reason about a
/// method call (leaky).
type ExpectedTypeResolver =
    dyn FnOnce(&mut TypeEnvironment, &MIRBaseMappings, &MIRType) -> CXResult<MIRExpression>;

pub struct ExpectedTypeDeferredExpr {
    resolver: Box<ExpectedTypeResolver>,
}

impl ExpectedTypeDeferredExpr {
    pub fn new<F>(resolver: F) -> Self
    where
        F: FnOnce(&mut TypeEnvironment, &MIRBaseMappings, &MIRType) -> CXResult<MIRExpression>
            + 'static,
    {
        Self {
            resolver: Box::new(resolver),
        }
    }

    fn resolve(
        self,
        env: &mut TypeEnvironment,
        base_data: &MIRBaseMappings,
        expected_type: &MIRType,
    ) -> CXResult<MIRExpression> {
        (self.resolver)(env, base_data, expected_type)
    }
}

impl std::fmt::Debug for ExpectedTypeDeferredExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ExpectedTypeDeferredExpr { .. }")
    }
}

#[derive(Debug)]
pub enum TypecheckedExpression {
    Ready(MIRExpression),
    IncompleteTemplatedCallee {
        name: QualifiedName,
        template_input: Option<CXTemplateInput>,
    },
    NeedsExpectedType(ExpectedTypeDeferredExpr),
}

impl TypecheckedExpression {
    fn as_ready(&self) -> CXResult<&MIRExpression> {
        match self {
            Self::Ready(expression) => Ok(expression),
            Self::IncompleteTemplatedCallee { name, .. } => CXError::create_result(format!(
                "Templated function '{}' requires an argument list for template deduction",
                name
            )),
            Self::NeedsExpectedType(_) => {
                CXError::create_result("Expression requires an expected type")
            }
        }
    }

    fn into_ready(self) -> CXResult<MIRExpression> {
        match self {
            Self::Ready(expression) => Ok(expression),
            Self::IncompleteTemplatedCallee { name, .. } => CXError::create_result(format!(
                "Templated function '{}' requires an argument list for template deduction",
                name
            )),
            Self::NeedsExpectedType(_) => {
                CXError::create_result("Expression requires an expected type")
            }
        }
    }

    fn into_ready_with_expected(
        self,
        env: &mut TypeEnvironment,
        base_data: &MIRBaseMappings,
        expected_type: &MIRType,
    ) -> CXResult<MIRExpression> {
        match self {
            Self::Ready(expression) => Ok(expression),
            Self::IncompleteTemplatedCallee { name, .. } => CXError::create_result(format!(
                "Templated function '{}' requires an argument list for template deduction",
                name
            )),
            Self::NeedsExpectedType(expr) => expr.resolve(env, base_data, expected_type),
        }
    }
}

#[derive(Debug)]
pub struct TypecheckResult {
    /// The accumulated expression
    pub expression: TypecheckedExpression,
    /// Implicit parameters carried upward for call sites (e.g. member receivers)
    pub implicit_parameters: Vec<MIRExpression>,
    /// Binding/place information for expressions that still denote a local place.
    pub binding: Option<TypecheckedBinding>,
    /// True when this value adopts an existing region instead of initializing a fresh one.
    pub adopting: bool,
}

impl From<MIRExpression> for TypecheckResult {
    fn from(expression: MIRExpression) -> Self {
        Self {
            expression: TypecheckedExpression::Ready(expression),
            implicit_parameters: Vec::new(),
            binding: None,
            adopting: false,
        }
    }
}

impl TypecheckResult {
    pub fn new_base(_type: MIRType, kind: MIRExpressionKind) -> Self {
        Self {
            expression: TypecheckedExpression::Ready(MIRExpression {
                token_range: None,
                kind,
                _type,
            }),
            implicit_parameters: Vec::new(),
            binding: None,
            adopting: false,
        }
    }

    pub fn incomplete_templated_callee(
        name: QualifiedName,
        template_input: Option<CXTemplateInput>,
    ) -> Self {
        Self {
            expression: TypecheckedExpression::IncompleteTemplatedCallee {
                name,
                template_input,
            },
            implicit_parameters: Vec::new(),
            binding: None,
            adopting: false,
        }
    }

    pub fn needs_expected_type<F>(resolver: F) -> Self
    where
        F: FnOnce(&mut TypeEnvironment, &MIRBaseMappings, &MIRType) -> CXResult<MIRExpression>
            + 'static,
    {
        Self {
            expression: TypecheckedExpression::NeedsExpectedType(ExpectedTypeDeferredExpr::new(
                resolver,
            )),
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

    pub fn decompose_function_expr(self) -> CXResult<(MIRExpression, Vec<MIRExpression>)> {
        Ok((self.expression.into_ready()?, self.implicit_parameters))
    }

    pub fn ensure_available(self, env: &mut TypeEnvironment) -> CXResult<Self> {
        if let Some(binding) = &self.binding {
            ensure_binding_available(
                env,
                self.expression.as_ready()?.token_range.clone(),
                &binding.root,
            )?
        }

        Ok(self)
    }

    /// Get the type of this typecheck result's expression
    pub fn get_type(&self) -> CXResult<MIRType> {
        Ok(self.expression.as_ready()?._type.clone())
    }

    pub fn get_type_if_ready(&self) -> CXResult<Option<MIRType>> {
        match &self.expression {
            TypecheckedExpression::Ready(expression) => Ok(Some(expression._type.clone())),
            TypecheckedExpression::NeedsExpectedType(_) => Ok(None),
            TypecheckedExpression::IncompleteTemplatedCallee { name, .. } => {
                CXError::create_result(format!(
                    "Templated function '{}' requires an argument list for template deduction",
                    name
                ))
            }
        }
    }

    pub fn set_token_range_if_missing(&mut self, token_range: TokenRange) -> CXResult<()> {
        let expression = match &mut self.expression {
            TypecheckedExpression::Ready(expression) => expression,
            TypecheckedExpression::IncompleteTemplatedCallee { .. }
            | TypecheckedExpression::NeedsExpectedType(_) => return Ok(()),
        };

        if expression.token_range.is_none() {
            expression.token_range = Some(token_range);
        }

        Ok(())
    }

    /// Extract the inner MIRExpression
    pub fn into_expression(self) -> CXResult<MIRExpression> {
        self.expression.into_ready()
    }

    pub fn into_expression_with_expected(
        self,
        env: &mut TypeEnvironment,
        base_data: &MIRBaseMappings,
        expected_type: &MIRType,
    ) -> CXResult<MIRExpression> {
        self.expression
            .into_ready_with_expected(env, base_data, expected_type)
    }
}
