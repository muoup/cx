use cx_mir::mir::data::{MIRIntegerType, MIRType, MIRTypeKind};
use cx_mir::mir::expression::{
    MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRFunctionContract, MIRUnOp,
};

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
}

impl From<MIRExpression> for TypecheckResult {
    fn from(expression: MIRExpression) -> Self {
        Self {
            expression,
            implicit_parameters: Vec::new(),
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
        }
    }

    pub fn with_implicit_parameters(mut self, implicit_parameters: Vec<MIRExpression>) -> Self {
        self.implicit_parameters = implicit_parameters;
        self
    }

    pub fn into_parts(self) -> (MIRExpression, Vec<MIRExpression>) {
        (self.expression, self.implicit_parameters)
    }

    pub fn chain(self, other: TypecheckResult) -> TypecheckResult {
        TypecheckResult::from(MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::Block {
                statements: vec![self.expression, other.expression],
            },
            _type: MIRType::unit(),
        })
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
        }
    }

    // Binary operations
    pub fn binary_op(lhs: Self, rhs: Self, op: MIRBinOp, result_type: MIRType) -> Self {
        TypecheckResult::new_base(
            result_type,
            MIRExpressionKind::BinaryOperation {
                lhs: Box::new(lhs.expression),
                rhs: Box::new(rhs.expression),
                op,
            },
        )
    }

    pub fn binary_op_raw(
        lhs: MIRExpression,
        rhs: MIRExpression,
        op: MIRBinOp,
        result_type: MIRType,
    ) -> Self {
        TypecheckResult::new_base(
            result_type,
            MIRExpressionKind::BinaryOperation {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            },
        )
    }

    // Unary operations
    pub fn unary_op(operand: Self, op: MIRUnOp, result_type: MIRType) -> Self {
        TypecheckResult::new_base(
            result_type,
            MIRExpressionKind::UnaryOperation {
                operand: Box::new(operand.expression),
                op,
            },
        )
    }

    // Type conversions
    pub fn type_conversion(operand: Self, conversion: MIRCoercion, result_type: MIRType) -> Self {
        TypecheckResult::new_base(
            result_type,
            MIRExpressionKind::TypeConversion {
                operand: Box::new(operand.expression),
                conversion,
            },
        )
    }

    // Memory operations
    pub fn memory_read(source: Self, _type: MIRType) -> Self {
        TypecheckResult::new_base(
            _type.clone(),
            MIRExpressionKind::MemoryRead {
                source: Box::new(source.expression),
            },
        )
    }

    pub fn memory_write(target: Self, value: Self, _type: MIRType) -> Self {
        TypecheckResult::new_base(
            _type.clone(),
            MIRExpressionKind::MemoryWrite {
                target: Box::new(target.expression),
                value: Box::new(value.expression),
            },
        )
    }

    pub fn copy_region(source: Self, _type: MIRType) -> Self {
        TypecheckResult::new_base(
            _type.clone(),
            MIRExpressionKind::RegionDuplicate {
                source: Box::new(source.expression),
                _type,
            },
        )
    }

    pub fn array_access(
        array: Self,
        index: Self,
        element_type: MIRType,
        result_type: MIRType,
    ) -> Self {
        TypecheckResult::new_base(
            result_type,
            MIRExpressionKind::ArrayAccess {
                array: Box::new(array.expression),
                index: Box::new(index.expression),
                element_type,
            },
        )
    }

    // Function calls
    pub fn call_function(
        function: Self,
        arguments: Vec<Self>,
        result_type: MIRType,
        _contract: MIRFunctionContract,
    ) -> Self {
        let (function, implicit_parameters) = function.into_parts();
        let arguments = implicit_parameters
            .into_iter()
            .chain(arguments.into_iter().map(|a| a.expression))
            .collect();

        TypecheckResult::new_base(
            result_type,
            MIRExpressionKind::CallFunction {
                function: Box::new(function),
                arguments,
            },
        )
    }

    // Tagged unions
    pub fn tagged_union_tag(value: Self, sum_type: MIRType) -> Self {
        TypecheckResult::new_base(
            MIRTypeKind::Integer {
                _type: MIRIntegerType::I8,
                signed: false,
            }
            .into(),
            MIRExpressionKind::TaggedUnionTag {
                value: Box::new(value.expression),
                sum_type,
            },
        )
    }

    pub fn tagged_union_get(value: Self, variant_type: MIRType, result_type: MIRType) -> Self {
        TypecheckResult::new_base(
            result_type,
            MIRExpressionKind::TaggedUnionGet {
                value: Box::new(value.expression),
                variant_type,
            },
        )
    }

    pub fn construct_tagged_union(variant_index: usize, value: Self, sum_type: MIRType) -> Self {
        TypecheckResult::new_base(
            sum_type.clone(),
            MIRExpressionKind::ConstructTaggedUnion {
                variant_index,
                value: Box::new(value.expression),
                sum_type,
            },
        )
    }

    // Control flow
    pub fn break_expr(scope_depth: usize) -> Self {
        TypecheckResult::new_base(MIRType::unit(), MIRExpressionKind::Break { scope_depth })
    }

    pub fn continue_expr(scope_depth: usize) -> Self {
        TypecheckResult::new_base(MIRType::unit(), MIRExpressionKind::Continue { scope_depth })
    }
}
