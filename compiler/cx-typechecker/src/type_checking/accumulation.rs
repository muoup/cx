use cx_typechecker_data::mir::expression::{MIRExpression, MIRExpressionKind, MIRValue};
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

    pub fn standard_expr(_type: MIRType, kind: MIRExpressionKind) -> Self {
        Self {
            expression: MIRExpression { kind, _type },
        }
    }

    pub fn chain(self, other: TypecheckResult) -> TypecheckResult {
        TypecheckResult::new(
            MIRExpression {
                kind: MIRExpressionKind::Block {
                    statements: vec![self.expression, other.expression],
                },
                _type: MIRType::unit()
            },
        )
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

    /// Temporary bridge: Convert TypecheckResult to MIRValue for legacy code
    /// TODO: Remove this after migration is complete
    pub fn into_mir_value(self) -> MIRValue {
        MIRValue::from_mir_expression(self.expression)
    }

    /// Temporary bridge: Create TypecheckResult from MIRValue for legacy code
    /// TODO: Remove this after migration is complete
    pub fn from_mir_value(value: MIRValue) -> Self {
        // Convert MIRValue to appropriate MIRExpression
        match value {
            MIRValue::BoolLiteral { value } => Self::new(MIRExpression {
                kind: MIRExpressionKind::BoolLiteral(value),
                _type: cx_typechecker_data::mir::types::MIRType::from(
                    cx_typechecker_data::mir::types::MIRTypeKind::Bool,
                ),
            }),
            MIRValue::IntLiteral {
                value,
                signed,
                _type,
            } => Self::new(MIRExpression {
                kind: MIRExpressionKind::IntLiteral(value, _type, signed),
                _type: cx_typechecker_data::mir::types::MIRType::from(
                    cx_typechecker_data::mir::types::MIRTypeKind::Integer { _type, signed },
                ),
            }),
            MIRValue::FloatLiteral { value, _type } => Self::new(MIRExpression {
                kind: MIRExpressionKind::FloatLiteral(value, _type),
                _type: cx_typechecker_data::mir::types::MIRType::from(
                    cx_typechecker_data::mir::types::MIRTypeKind::Float { _type },
                ),
            }),
            MIRValue::NULL => Self::new(MIRExpression {
                kind: MIRExpressionKind::Null,
                _type: cx_typechecker_data::mir::types::MIRType::unit(),
            }),
            MIRValue::Parameter { name, _type } => Self::new(MIRExpression {
                kind: MIRExpressionKind::Parameter(name),
                _type: _type,
            }),
            MIRValue::GlobalValue { name, _type } => Self::new(MIRExpression {
                kind: MIRExpressionKind::GlobalVariable(name),
                _type: _type,
            }),
            MIRValue::FunctionReference { prototype, .. } => Self::new(MIRExpression {
                kind: MIRExpressionKind::FunctionReference { prototype },
                _type: prototype.return_type.pointer_to(),
            }),
            MIRValue::Register { register, _type } => Self::new(MIRExpression {
                kind: MIRExpressionKind::LocalVariable(register.name),
                _type: _type,
            }),
        }
    }

    /// Map expression using a transformation function
    pub fn expression_map<F>(&self, f: F) -> TypecheckResult
    where
        F: FnOnce(&MIRExpression) -> MIRExpression,
    {
        TypecheckResult {
            expression: f(&self.expression),
        }
    }

    /// Map expression kind using a transformation function
    pub fn map_kind<F>(self, f: F) -> TypecheckResult
    where
        F: FnOnce(MIRExpressionKind) -> MIRExpressionKind,
    {
        TypecheckResult {
            expression: MIRExpression {
                kind: f(self.expression.kind),
                _type: self.expression._type,
            },
        }
    }
}
