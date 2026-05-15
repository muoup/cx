use cx_mir::mir::data::MIRType;
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

use crate::environment::TypeEnvironment;
use crate::type_checking::value::locals::ensure_binding_available;

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
#[derive(Debug, Clone)]
pub struct TypecheckResult {
    /// The accumulated expression
    pub expression: MIRExpression,
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
            expression,
            implicit_parameters: Vec::new(),
            binding: None,
            adopting: false,
        }
    }
}

impl TypecheckResult {
    pub fn new_base(_type: MIRType, kind: MIRExpressionKind) -> Self {
        Self {
            expression: MIRExpression {
                token_range: None,
                kind,
                _type,
            },
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

    pub fn decompose_function_expr(self) -> (MIRExpression, Vec<MIRExpression>) {
        (self.expression, self.implicit_parameters)
    }

    pub fn ensure_available(self, env: &mut TypeEnvironment) -> CXResult<Self> {
        if let Some(binding) = &self.binding {
            ensure_binding_available(env, self.expression.token_range.clone(), &binding.root)?
        }

        Ok(self)
    }

    /// Get the type of this typecheck result's expression
    pub fn get_type(&self) -> MIRType {
        self.expression._type.clone()
    }

    /// Extract the inner MIRExpression
    pub fn into_expression(self) -> MIRExpression {
        self.expression
    }
}
