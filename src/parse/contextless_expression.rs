use std::env::args;
use std::{clone, iter};
use std::task::Context;
use crate::lex::token::OperatorType;
use crate::log_error;
use crate::parse::ast::{Expression, LValueExpression, RValueExpression, ValueType, VarInitialization};
use crate::parse::expression::{parse_lvalue, parse_rvalue};
use crate::parse::parser::ParserData;
use crate::util::MaybeResult;

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
            Some(Expression::LValue(LValueExpression::Identifier(name))),

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
        } if matches!(right.as_ref(), ContextlessExpression::Identifier(_)) => {
            let left = contextualize_lvalue(data, *left)?;
            let ContextlessExpression::Identifier(field_name) = *right else { unreachable!(); };

            Some(
                Expression::LValue(
                    LValueExpression::StructField {
                        struct_: Box::new(left),
                        field_name
                    }
                )
            )
        },

        ContextlessExpression::UnambiguousExpression(expr) => Some(expr),

        _ =>
            Some(
                Expression::LValue(
                    LValueExpression::Initialization(detangle_initialization(expr)?)
                )
            ),
    }
}

pub(crate) fn maybe_contextualize_rvalue(data: &mut ParserData, expr: ContextlessExpression) -> MaybeResult<Expression, ContextlessExpression, ()> {
    match expr {
        ContextlessExpression::Identifier(name) =>
            MaybeResult::Consumed(
                Expression::RValue(
                    RValueExpression::LoadedLValue {
                        lvalue: Box::new(Expression::LValue(LValueExpression::Identifier(name)))
                    }
                )
            ),

        ContextlessExpression::FunctionCall {
            reference, args
        } => {
            let ContextlessExpression::Identifier(name) = *reference else {
                unimplemented!("Function call with non-identifier reference: {:#?}", reference);
            };
            let Some(args) = args.iter()
                .map(|expr| contextualize_rvalue(data, expr.clone()))
                .collect::<Option<Vec<_>>>() else {
                return MaybeResult::Error(());
            };

            MaybeResult::Consumed(
                Expression::RValue(
                    RValueExpression::DirectFunctionCall {
                        name, args
                    }
                )
            )
        },

        ContextlessExpression::BinaryOperation {
            op: OperatorType::Access,
            left, right
        } if matches!(right.as_ref(), ContextlessExpression::FunctionCall { .. }) => {
            let ContextlessExpression::FunctionCall { reference, args } = *right else {
                unreachable!();
            };
            let ContextlessExpression::Identifier(fn_name) = *reference else {
                panic!("Expected identifier for member function call, found: {:#?}", reference);
            };
            let Some(struct_) = contextualize_rvalue(data, *left) else {
                println!("Failed to contextualize struct for member function call");
                return MaybeResult::Error(());
            };
            let Some(args) = args.iter()
                .map(|expr| contextualize_rvalue(data, expr.clone()))
                .collect::<Option<Vec<_>>>() else {
                return MaybeResult::Error(());
            };

            MaybeResult::Consumed(
                Expression::RValue(
                    RValueExpression::MemberFunctionCall {
                        struct_parent: Box::new(struct_),
                        name: fn_name,
                        args
                    }
                )
            )
        }

        ContextlessExpression::BinaryOperation {
            op: OperatorType::Access, ..
        } => {
            let Some(lvalue) = contextualize_lvalue(data, expr) else {
                return MaybeResult::Error(());
            };

            MaybeResult::Consumed(
                Expression::RValue(
                    RValueExpression::LoadedLValue {
                        lvalue: Box::new(lvalue)
                    }
                )
            )
        },

        ContextlessExpression::BinaryOperation {
            op, left, right
        } => {
            let Some(left) = contextualize_rvalue(data, *left) else {
                return MaybeResult::Error(());
            };
            let Some(right) = contextualize_rvalue(data, *right) else {
                return MaybeResult::Error(());
            };

            MaybeResult::Consumed(
                Expression::RValue(
                    RValueExpression::BinaryOperation {
                        operator: op,
                        left: Box::new(left),
                        right: Box::new(right)
                    }
                )
            )
        },

        ContextlessExpression::UnaryOperation {
            op: OperatorType::Multiply,
            operand
        } => {
            let Some(operand) = contextualize_rvalue(data, *operand) else {
                return MaybeResult::Error(());
            };

            MaybeResult::Consumed(
                Expression::RValue(
                    RValueExpression::UnaryOperation {
                        operator: OperatorType::Multiply,
                        operand: Some(Box::new(operand))
                    }
                )
            )
        },

        ContextlessExpression::UnambiguousExpression(expr) => MaybeResult::Consumed(expr),

        expr => MaybeResult::Unconsumed(expr)
    }
}

pub(crate) fn contextualize_rvalue(data: &mut ParserData, expr: ContextlessExpression) -> Option<Expression> {
    match maybe_contextualize_rvalue(data, expr) {
        MaybeResult::Consumed(expr) => Some(expr),
        MaybeResult::Unconsumed(expr) => {
            log_error!("Invalid rvalue: {:#?}", expr);
        },
        MaybeResult::Error(err) => {
            log_error!("Error contextualizing rvalue: {:#?}", err);
        }
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