use cx_typechecker_data::mir::expression::{MIRExpression, MIRExpressionKind, MIRBinOp, MIRUnOp, MIRCoercion, MIRFunctionContract};
use cx_typechecker_data::mir::types::{CXIntegerType, MIRType, MIRTypeKind};

/// Result of typechecking an expression/statement
#[derive(Debug, Clone)]
pub struct TypecheckResult {
    /// The accumulated expression
    pub expression: MIRExpression,
}

impl TypecheckResult {
    pub fn expr2(expression: MIRExpression) -> Self {
        Self { expression }
    }

    pub fn expr(_type: MIRType, kind: MIRExpressionKind) -> Self {
        Self {
            expression: MIRExpression { kind, _type },
        }
    }

    pub fn chain(self, other: TypecheckResult) -> TypecheckResult {
        TypecheckResult::expr2(
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

    // Binary operations
    pub fn binary_op(lhs: Self, rhs: Self, op: MIRBinOp, result_type: MIRType) -> Self {
        TypecheckResult::expr(
            result_type,
            MIRExpressionKind::BinaryOperation {
                lhs: Box::new(lhs.expression),
                rhs: Box::new(rhs.expression),
                op,
            }
        )
    }

    pub fn binary_op_raw(lhs: MIRExpression, rhs: MIRExpression, op: MIRBinOp, result_type: MIRType) -> Self {
        TypecheckResult::expr(
            result_type,
            MIRExpressionKind::BinaryOperation {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            }
        )
    }

    // Unary operations
    pub fn unary_op(operand: Self, op: MIRUnOp, result_type: MIRType) -> Self {
        TypecheckResult::expr(
            result_type,
            MIRExpressionKind::UnaryOperation {
                operand: Box::new(operand.expression),
                op,
            }
        )
    }

    // Type conversions
    pub fn type_conversion(operand: Self, conversion: MIRCoercion, result_type: MIRType) -> Self {
        TypecheckResult::expr(
            result_type,
            MIRExpressionKind::TypeConversion {
                operand: Box::new(operand.expression),
                conversion,
            }
        )
    }

    // Memory operations
    pub fn memory_read(source: Self, _type: MIRType) -> Self {
        TypecheckResult::expr(
            _type.clone(),
            MIRExpressionKind::MemoryRead {
                source: Box::new(source.expression)
            }
        )
    }

    pub fn memory_write(target: Self, value: Self, _type: MIRType) -> Self {
        TypecheckResult::expr(
            _type.clone(),
            MIRExpressionKind::MemoryWrite {
                target: Box::new(target.expression),
                value: Box::new(value.expression),
            }
        )
    }

    pub fn copy_region(source: Self, _type: MIRType) -> Self {
        TypecheckResult::expr(
            _type.clone(),
            MIRExpressionKind::CopyRegion {
                source: Box::new(source.expression),
                _type,
            }
        )
    }

    pub fn array_access(array: Self, index: Self, element_type: MIRType, result_type: MIRType) -> Self {
        TypecheckResult::expr(
            result_type,
            MIRExpressionKind::ArrayAccess {
                array: Box::new(array.expression),
                index: Box::new(index.expression),
                element_type,
            }
        )
    }

    // Function calls
    pub fn call_function(function: Self, arguments: Vec<Self>, result_type: MIRType, contract: MIRFunctionContract) -> Self {
        TypecheckResult::expr(
            result_type,
            MIRExpressionKind::CallFunction {
                function: Box::new(function.expression),
                arguments: arguments.into_iter().map(|a| a.expression).collect(),
            }
        )
    }

    // Tagged unions
    pub fn tagged_union_tag(value: Self, sum_type: MIRType) -> Self {
        TypecheckResult::expr(
            MIRTypeKind::Integer {
                _type: CXIntegerType::I8,
                signed: false,
            }.into(),
            MIRExpressionKind::TaggedUnionTag {
                value: Box::new(value.expression),
                sum_type,
            }
        )
    }

    pub fn tagged_union_get(value: Self, variant_type: MIRType, result_type: MIRType) -> Self {
        TypecheckResult::expr(
            result_type,
            MIRExpressionKind::TaggedUnionGet {
                value: Box::new(value.expression),
                variant_type,
            }
        )
    }

    pub fn construct_tagged_union(variant_index: usize, value: Self, sum_type: MIRType) -> Self {
        TypecheckResult::expr(
            sum_type.clone(),
            MIRExpressionKind::ConstructTaggedUnion {
                variant_index,
                value: Box::new(value.expression),
                sum_type,
            }
        )
    }

    // Control flow
    pub fn break_expr(scope_depth: usize) -> Self {
        TypecheckResult::expr(
            MIRType::unit(),
            MIRExpressionKind::Break { scope_depth },
        )
    }

    pub fn continue_expr(scope_depth: usize) -> Self {
        TypecheckResult::expr(
            MIRType::unit(),
            MIRExpressionKind::Continue { scope_depth },
        )
    }
}
