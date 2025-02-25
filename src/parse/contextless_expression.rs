use std::task::Context;
use crate::lex::token::OperatorType;
use crate::log_error;
use crate::parse::ast::{Expression, FunctionParameter, LValueExpression, ValueExpression, ValueType, VarInitialization};
use crate::parse::expression::{parse_lvalue, parse_rvalue};
use crate::parse::parser::ParserData;

#[derive(Debug, Clone)]
pub(crate) enum ContextlessExpression {
    BinaryOperation {
        op: OperatorType,
        left: Box<ContextlessExpression>,
        right: Box<ContextlessExpression>
    },
    UnaryOperation {
        op: OperatorType,
        operand: Box<ContextlessExpression>
    },

    CompoundExpression {
        left: Box<ContextlessExpression>,
        right: Box<ContextlessExpression>
    },
    FunctionCall {
        reference: Box<ContextlessExpression>,
        args: Vec<ContextlessExpression>
    },

    UnambiguousExpression(Expression),
    Identifier(String),
}

pub(crate) fn contextualize_lvalue(data: &mut ParserData, expr: ContextlessExpression) -> Option<Expression> {
    match expr {
        // Case 1 - Non-declarative lvalue (i.e. variable_reference)
        ContextlessExpression::Identifier(name) =>
            Some(Expression::Identifier(name)),

        ContextlessExpression::UnaryOperation {
            op: OperatorType::Multiply, operand
        } => Some(
            Expression::LValue(
                LValueExpression::DereferencedPointer {
                    pointer: Box::new(contextualize_rvalue(data, *operand)?)
                }
            )
        ),

        ContextlessExpression::BinaryOperation {
            op: OperatorType::Access,
            left, right
        } => Some(
            Expression::Value(
                ValueExpression::BinaryOperation {
                    operator: OperatorType::Access,
                    left: Box::new(contextualize_rvalue(data, *left)?),
                    right: Box::new(contextualize_rvalue(data, *right)?)
                }
            )
        ),

        ContextlessExpression::UnambiguousExpression(expr) => Some(expr),

        _ =>
            Some(
                Expression::LValue(
                    LValueExpression::Initialization(detangle_initialization(expr)?)
                )
            ),
    }
}

pub(crate) fn contextualize_rvalue(data: &mut ParserData, expr: ContextlessExpression) -> Option<Expression> {
    match expr {
        ContextlessExpression::Identifier(name) =>
            Some(Expression::Identifier(name)),

        ContextlessExpression::BinaryOperation {
            op, left, right
        } => {
            Some(
                Expression::Value(
                    ValueExpression::BinaryOperation {
                        operator: op,
                        left: Box::new(contextualize_rvalue(data, *left)?),
                        right: Box::new(contextualize_rvalue(data, *right)?)
                    }
                )
            )
        },

        ContextlessExpression::UnaryOperation {
            op: OperatorType::Multiply,
            operand
        } => Some(
            Expression::Value(
                ValueExpression::UnaryOperation {
                    operator: OperatorType::Multiply,
                    operand: Some(Box::new(contextualize_rvalue(data, *operand)?))
                }
            )
        ),

        ContextlessExpression::FunctionCall {
            reference, args
        } => {
            let ContextlessExpression::Identifier(name) = *reference else {
                unimplemented!("Function call with non-identifier reference: {:#?}", reference);
            };

            Some(
                Expression::Value(
                    ValueExpression::DirectFunctionCall {
                        name,
                        args: args.iter()
                            .map(|expr| contextualize_rvalue(data, expr.clone()))
                            .collect::<Option<Vec<_>>>()?
                    }
                )
            )
        },

        ContextlessExpression::UnambiguousExpression(expr) => Some(expr),

        _ => contextualize_lvalue(data, expr)
    }
}

pub(crate) fn detangle_initialization(expr: ContextlessExpression) -> Option<VarInitialization> {
    let (type_, body) = detangle_typed_expr(expr)?;

    let ContextlessExpression::Identifier(name) = body else {
        log_error!("Expected identifier for initialization, found: {:#?}", body);
    };

    Some(VarInitialization {
        type_,
        name
    })
}

pub(crate) fn detangle_typed_expr(expr: ContextlessExpression) -> Option<(ValueType, ContextlessExpression)> {
    let (left, right) = match expr {
        ContextlessExpression::BinaryOperation {
            op: OperatorType::Multiply,
            left, right
        } => (
            left,
            ContextlessExpression::UnaryOperation {
                op: OperatorType::Multiply,
                operand: right
            }
        ),

        ContextlessExpression::CompoundExpression {
            left, right
        } => (left, *right),

        _ => {
            log_error!("Invalid expression for declaration detangling, found: {:#?}", expr);
        }
    };

    let ContextlessExpression::Identifier(name) = *left else {
        log_error!("Expected identifier for declaration, found: {:#?}", left);
    };

    coalesce_type(
        ValueType::Identifier(name),
        right
    )
}

pub(crate) fn coalesce_type(left: ValueType, right: ContextlessExpression) -> Option<(ValueType, ContextlessExpression)> {
    match right {
        ContextlessExpression::UnaryOperation {
            op: OperatorType::Multiply,
            operand
        } => coalesce_type(ValueType::PointerTo(Box::new(left)), *operand),

        _ => Some((left, right))
    }
}