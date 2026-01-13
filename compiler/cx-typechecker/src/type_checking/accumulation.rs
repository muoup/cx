use cx_typechecker_data::mir::expression::MIRExpressionKind;
use cx_typechecker_data::mir::types::MIRType;

/// Result of typechecking an expression/statement
#[derive(Debug, Clone)]
pub struct TypecheckResult {
    /// The accumulated expression
    pub expression: MIRExpression,
}

impl TypecheckResult {
    pub fn new(expression: MIRExpression) -> Self {
        Self { expression }
    }

    /// Get the type of this typecheck result's expression
    /// TODO: Implement proper type inference for MIRExpression
    pub fn get_type(&self) -> MIRType {
        // For now, return unit type as placeholder
        // Will need to implement expression type inference
        MIRType::unit()
    }
    
    pub fn expression_map<F>(&self, f: F) -> TypecheckResult
    where
        F: FnOnce(&MIRExpression) -> MIRExpression,
    {
        TypecheckResult {
            expression: f(&self.expression),
        }
    }
}
