use cx_mir::mir::data::MIRType;
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind, MIRFunctionContract};

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
    pub contract: Option<MIRFunctionContract>,
}

impl From<MIRExpression> for TypecheckResult {
    fn from(expression: MIRExpression) -> Self {
        Self {
            expression,
            implicit_parameters: Vec::new(),
            contract: None
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
            contract: None
        }
    }

    pub fn with_implicit_parameters(mut self, implicit_parameters: Vec<MIRExpression>) -> Self {
        self.implicit_parameters = implicit_parameters;
        self
    }
    
    pub fn with_contract(mut self, contract: Option<MIRFunctionContract>) -> Self {
        self.contract = contract;
        self
    }

    pub fn decompose_function_expr(self) -> (MIRExpression, Vec<MIRExpression>, Option<MIRFunctionContract>) {
        (self.expression, self.implicit_parameters, self.contract)
    }

    /// Get the type of this typecheck result's expression
    pub fn get_type(&self) -> MIRType {
        self.expression._type.clone()
    }

    /// Extract the inner MIRExpression
    pub fn into_expression(self) -> MIRExpression {
        self.expression
    }

    /// Extract the inner MIRExpressionKind
    pub fn into_kind(self) -> MIRExpressionKind {
        self.expression.kind
    }

    /// Map expression using a transformation function
    pub fn expression_map<F>(&self, f: F) -> TypecheckResult
    where
        F: FnOnce(&MIRExpression) -> MIRExpression,
    {
        TypecheckResult {
            expression: f(&self.expression),
            implicit_parameters: self.implicit_parameters.clone(),
            contract: self.contract.clone()
        }
    }

    /// Map expression kind using a transformation function
    pub fn map_kind<F>(self, f: F) -> TypecheckResult
    where
        F: FnOnce(MIRExpressionKind) -> MIRExpressionKind,
    {
        TypecheckResult {
            expression: MIRExpression {
                token_range: None,
                kind: f(self.expression.kind),
                _type: self.expression._type,
            },
            implicit_parameters: self.implicit_parameters,
            contract: self.contract
        }
    }
}
