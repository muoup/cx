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
}

pub enum TypecheckExtract<T> {
    Fail(TypecheckResult),
    Succ(T),
}

impl<T> TypecheckExtract<T> {
    pub fn into_result(self) -> CXResult<T> {
        match self {
            Self::Succ(value) => Ok(value),
            Self::Fail(result) => result
                .expression
                .into_ready()
                .and_then(|_| CXError::create_result("Typecheck result could not be extracted")),
        }
    }
}

pub struct CalleeExtraction {
    pub function: MIRExpression,
    pub implicit_args: Vec<MIRExpression>,
    pub deduction_arg_prefix: Vec<MIRType>,
}

impl CalleeExtraction {
    pub fn new(
        function: MIRExpression,
        implicit_args: Vec<MIRExpression>,
        deduction_arg_prefix: Vec<MIRType>,
    ) -> Self {
        Self {
            function,
            implicit_args,
            deduction_arg_prefix,
        }
    }
}

#[derive(Debug)]
pub struct TypecheckResult {
    /// The accumulated expression
    expression: TypecheckedExpression,
    /// Implicit parameters carried upward for call sites (e.g. member receivers)
    implicit_parameters: Vec<MIRExpression>,
    /// Types that participate in function template deduction but are not ordinary explicit arguments.
    deduction_arg_prefix: Vec<MIRType>,
    /// Binding/place information for expressions that still denote a local place.
    binding: Option<TypecheckedBinding>,
    /// True when this value adopts an existing region instead of initializing a fresh one.
    adopting: bool,
}

impl From<MIRExpression> for TypecheckResult {
    fn from(expression: MIRExpression) -> Self {
        Self {
            expression: TypecheckedExpression::Ready(expression),
            implicit_parameters: Vec::new(),
            deduction_arg_prefix: Vec::new(),
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
            deduction_arg_prefix: Vec::new(),
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
            deduction_arg_prefix: Vec::new(),
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
            deduction_arg_prefix: Vec::new(),
            binding: None,
            adopting: false,
        }
    }

    pub fn with_implicit_parameters(mut self, implicit_parameters: Vec<MIRExpression>) -> Self {
        self.implicit_parameters = implicit_parameters;
        self
    }

    pub fn with_deduction_arg_prefix(mut self, deduction_arg_prefix: Vec<MIRType>) -> Self {
        self.deduction_arg_prefix = deduction_arg_prefix;
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

    pub fn try_into_expression(self) -> TypecheckExtract<MIRExpression> {
        match self.expression {
            TypecheckedExpression::Ready(expression) => TypecheckExtract::Succ(expression),
            expression => TypecheckExtract::Fail(Self { expression, ..self }),
        }
    }

    pub fn try_into_callee(self) -> TypecheckExtract<CalleeExtraction> {
        match self.expression {
            TypecheckedExpression::Ready(function) => TypecheckExtract::Succ(CalleeExtraction::new(
                function,
                self.implicit_parameters,
                self.deduction_arg_prefix,
            )),
            expression => TypecheckExtract::Fail(Self { expression, ..self }),
        }
    }

    pub fn into_incomplete_callee_parts(
        self,
    ) -> Option<(
        QualifiedName,
        Option<CXTemplateInput>,
        Vec<MIRType>,
        Vec<MIRExpression>,
    )> {
        match self.expression {
            TypecheckedExpression::IncompleteTemplatedCallee {
                name,
                template_input,
            } => Some((
                name,
                template_input,
                self.deduction_arg_prefix,
                self.implicit_parameters,
            )),
            _ => None,
        }
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
        self.try_into_expression().into_result()
    }

    pub fn apply_expected_type(
        self,
        env: &mut TypeEnvironment,
        base_data: &MIRBaseMappings,
        expected_type: &MIRType,
    ) -> CXResult<Self> {
        match self.expression {
            TypecheckedExpression::NeedsExpectedType(expr) => Ok(Self {
                expression: TypecheckedExpression::Ready(expr.resolve(
                    env,
                    base_data,
                    expected_type,
                )?),
                implicit_parameters: self.implicit_parameters,
                deduction_arg_prefix: self.deduction_arg_prefix,
                binding: self.binding,
                adopting: self.adopting,
            }),
            _ => Ok(self),
        }
    }

    pub fn into_expression_with_expected(
        self,
        env: &mut TypeEnvironment,
        base_data: &MIRBaseMappings,
        expected_type: &MIRType,
    ) -> CXResult<MIRExpression> {
        self.apply_expected_type(env, base_data, expected_type)?
            .into_expression()
    }
}
