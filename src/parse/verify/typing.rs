use std::rc::Rc;
use crate::lex::token::OperatorType;
use crate::parse::ast::{Expression, LValueExpression, UnverifiedExpression, ValueType};
use crate::parse::verify::context::VerifyContext;
use crate::parse::verify::local_pass::{ExprVerifyResult, VerifyResult};

/**
 *  Fixes some context-specific parsing ambiguities related to l-values.
 *  Namely, an expression such as int *x will be read as (int) * (x), which is not what we want,
 *  but we can only know that after we know we are parsing an l-value.
 *
 *  Will not throw an error if the formatting is not applicable.
 */
pub(crate) fn format_lvalue(context: &mut VerifyContext, expr: &mut Expression) -> VerifyResult<()> {
    match expr {
        // (type) * (identifier) -> type (*identifier)
        Expression::Unverified(
            UnverifiedExpression::BinaryOperation {
                operator: OperatorType::Multiply,
                left, right
            }
        ) => {
            *expr = Expression::Unverified(
                UnverifiedExpression::CompoundExpression {
                    prefix: left.clone(),
                    suffix: Box::new(
                        Expression::Unverified(
                            UnverifiedExpression::UnaryOperation {
                                operator: OperatorType::Dereference,
                                operand: right.clone()
                            }
                        )
                    )
                }
            );

            format_lvalue(context, expr);
        },

        Expression::Unverified(
            UnverifiedExpression::CompoundExpression { prefix, suffix }
        ) => {
            let (type_, name) = verify_compound_pair(context, prefix, suffix)?;

            *expr = Expression::LValue(
                LValueExpression::Alloca {
                    type_: type_.clone(),
                    name: name.clone(),
                }
            );
        },

        _ => ()
    };

    Some(())
}

pub(crate) fn verify_type(context: &mut VerifyContext, type_: &mut ValueType) -> VerifyResult<()> {
    match type_ {
        ValueType::Unverified(name) => {
            let Some(verified_type) = context.types_table.get(name).cloned() else {
                println!("Unknown type: {:?}", name);
                return None
            };

            *type_ = verified_type;

            Some(())
        },

        // Yuck
        ValueType::PointerTo(inner) => {
            let mut inner_clone = inner.as_ref().clone();
            verify_type(context, &mut inner_clone)?;
            *inner = Rc::new(inner_clone);

            Some(())
        },
        ValueType::Array { type_: inner, .. } => {
            let mut inner_clone = inner.as_ref().clone();
            verify_type(context, &mut inner_clone)?;
            *inner = Rc::new(inner_clone);

            Some(())
        },
        _ => Some(())
    }
}

pub(crate) fn verify_compound_expression(context: &mut VerifyContext, expression: &mut Expression) -> ExprVerifyResult {
    let Expression::Unverified(
        UnverifiedExpression::CompoundExpression { prefix, suffix }
    ) = expression else {
        panic!("verify_compound_expression reached with: {:?}", expression)
    };

    let (val_type, name) = verify_compound_pair(context, prefix, suffix)?;

    *expression = Expression::LValue(
        LValueExpression::Alloca {
            name,
            type_: val_type.clone()
        }
    );

    Some(val_type)
}

pub(crate) fn verify_compound_pair(context: &mut VerifyContext, prefix: &mut Expression, suffix: &mut Expression) -> VerifyResult<(ValueType, String)> {
    let Expression::Unverified(
        UnverifiedExpression::Identifier(name)
    ) = prefix else {
        println!("Compound expressions must begin with an identifier, found: {:?}", suffix);
        return None
    };

    let (mut val_type, name) =
        coalesce_typed_identifier(context, ValueType::Unverified(name.clone()), suffix)?;

    verify_type(context, &mut val_type)?;

    Some((val_type, name))
}

pub(crate) fn coalesce_typed_identifier(context: &mut VerifyContext, l_type: ValueType, mut expr_header: &Expression) -> VerifyResult<(ValueType, String)> {
    match expr_header {
        Expression::Unverified(
            UnverifiedExpression::Identifier(name)
        ) => Some((l_type, name.clone())),

        Expression::Unverified(
            UnverifiedExpression::UnaryOperation {
                operator: OperatorType::Dereference,
                operand
            }
        ) => coalesce_typed_identifier(context, ValueType::PointerTo(Rc::new(l_type)), operand),

        _ => {
            println!("Expected identifier or dereference, found: {:?}", expr_header);
            None
        }
    }
}