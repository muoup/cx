use std::{clone, iter};
use crate::lex::token::OperatorType;
use crate::parse::ast::{ControlExpression, Expression, GlobalStatement, IntegerCastType, LValueExpression, LiteralExpression, UnverifiedExpression, ValueExpression, ValueType};
use crate::parse::verify::context::{FunctionPrototype, VerifyContext};
use crate::parse::verify::typing::{format_lvalue, get_struct_field_offset, verify_compound_pair, verify_type};
use std::rc::Rc;

pub(crate) type VerifyResult<T> = Option<T>;
pub(crate) type ExprVerifyResult = Option<ValueType>;

pub(crate) fn local_pass(context: &mut VerifyContext, stmts: &mut Vec<GlobalStatement>) -> VerifyResult<()> {
    for stmt in stmts {
        verify_global_statement(context, stmt)?;
    }

    Some(())
}

fn verify_global_statement(context: &mut VerifyContext, stmt: &mut GlobalStatement) -> VerifyResult<()> {
    match stmt {
        GlobalStatement::Function {
            prototype, body
        } => {
            context.push_scope();

            for param in prototype.args.iter() {
                context.insert_variable(param.name.clone(), param.type_.clone());
            }

            context.current_return_type = Some(prototype.return_type.clone());

            if let Some(body) = body {
                for expr in body.iter_mut() {
                    let Some(_) = verify_expression(context, expr) else {
                        println!("Failed to verify expression: {:#?}", expr);
                        return None
                    };
                }
            }

            context.pop_scope();
        },

        _ => {
            println!("Unimplemented global statement {:?}", stmt);
        }
    }

    Some(())
}

fn verify_expression(context: &mut VerifyContext, expr: &mut Expression) -> ExprVerifyResult {
    match expr {
        Expression::Control(control) => verify_control_expr(context, control),
        Expression::Value(value) => verify_val_expr(context, value),
        Expression::Unverified(unverified) => {
            *expr = characterize_unverified_expr(context, unverified)?;
            verify_expression(context, expr)
        },
        Expression::LValue(_) => verify_lvalue(context, expr),
        Expression::Literal(lit) => {
            match lit {
                LiteralExpression::IntLiteral { bytes, .. }
                    => Some(ValueType::Integer { bytes: *bytes, signed: true }),
                LiteralExpression::FloatLiteral { bytes, .. }
                    => Some(ValueType::Float { bytes: *bytes }),
                LiteralExpression::StringLiteral { .. } => {
                    let _char = context.get_type("char")?;
                    Some(ValueType::PointerTo(Rc::new(_char)))
                }
            }
        }

        _ => {
            println!("Unimplemented expression {:?}", expr);
            None
        },
    }
}

pub(crate) fn verify_lvalue(context: &mut VerifyContext, expr: &mut Expression) -> ExprVerifyResult {
    format_lvalue(context, expr)?;

    match expr {
        Expression::LValue(lvalue) => {
            match lvalue {
                LValueExpression::Alloca { type_, .. } => {
                    verify_type(context, type_)?;
                    Some(type_.clone())
                },
                LValueExpression::Value { name } => {
                    let Some(type_) = context.get_variable(name.as_ref()) else {
                        println!("Variable {} not found", name);
                        return None
                    };

                    Some(type_.clone())
                },
                _ => unimplemented!("Unimplemented l-value: {:?}", lvalue)
            }
        },
        Expression::Unverified (
            UnverifiedExpression::BinaryOperation {
                left, right,
                operator: OperatorType::Access
            }
        ) => {
            let _type = verify_lvalue(context, left)?;
            let ValueType::Structured { fields } = _type.clone() else {
                println!("Cannot access fields of non-struct type: {:?}", _type);
                return None
            };
            let Expression::Unverified(UnverifiedExpression::Identifier(right)) = right.as_ref() else {
                println!("Field access requires an identifier, found: {:?}", right);
                return None
            };

            let Some(field) = fields.iter().find(|(name, _)| name == right) else {
                println!("Field {} not found in struct", right);
                return None
            };

            // ptr = structure pointer + field offset
            // let mut ptr = Box::new(
            //     Expression::Value(
            //         ValueExpression::BinaryOperation {
            //             left: left.to_owned(),
            //             right: Box::new(
            //                 Expression::Literal(
            //                     LiteralExpression::IntLiteral {
            //                         val: get_struct_field_offset(context, _type, right)? as i64,
            //                         bytes: 8
            //                     }
            //                 )
            //             ),
            //             operator: OperatorType::Add
            //         }
            //     )
            // );

            *expr = Expression::LValue(
                LValueExpression::StructField {
                    struct_: left.to_owned(),
                    field_offset: get_struct_field_offset(context, _type, right)?,
                    field_type: field.1.clone()
                }
            );

            Some(field.1.clone())
        },

        _ => {
            println!("Invalid l-value: {:?}", expr);
            None
        },
    }
}

fn verify_control_expr(context: &mut VerifyContext, expr: &mut ControlExpression) -> ExprVerifyResult {
    match expr {
        ControlExpression::Return(expr) => {
            let target_type = context.current_return_type.as_ref().cloned()?;
            verify_and_coerce(context, expr, &target_type)?;
        }
        ControlExpression::If {
            condition, then,
            else_
        } => {
            verify_expression(context, condition)?;

            context.push_scope();
            for expr in then.iter_mut() {
                verify_expression(context, expr);
            }
            context.pop_scope();

            context.push_scope();
            for expr in else_.iter_mut() {
                verify_expression(context, expr);
            }
            context.pop_scope();
        },
        ControlExpression::ForLoop {
            init, condition,
            increment, body
        } => {
            verify_expression(context, init);
            verify_expression(context, condition);
            verify_expression(context, increment);

            context.push_scope();
            for expr in body.iter_mut() {
                verify_expression(context, expr);
            }
            context.pop_scope();
        },
        ControlExpression::Loop {
            condition, body, ..
        } => {
            verify_expression(context, condition);
            context.push_scope();
            for expr in body.iter_mut() {
                verify_expression(context, expr);
            }
            context.pop_scope();
        },
        _ => println!("Unimplemented control expression {:?}", expr),
    };

    Some(ValueType::Unit)
}

fn verify_val_expr(context: &mut VerifyContext, expr: &mut ValueExpression) -> Option<ValueType> {
    match expr {
        ValueExpression::DirectFunctionCall {
            args, name
        } => {
            let Some(func) = context.get_function(&name) else {
                println!("Function {} not found", name);
                return None
            };
            let FunctionPrototype { return_type, args: intended_args, .. } = func.as_ref();

            for (arg, intended_type) in args.iter_mut().zip(intended_args.iter()) {
                verify_and_coerce(context, arg, &intended_type.type_)?;
            }

            Some(return_type.clone())
        },
        ValueExpression::UnaryOperation { operand, .. } => {
            if let Some(operand) = operand {
                verify_expression(context, operand)
            } else {
                None
            }
        },
        ValueExpression::BinaryOperation { left, right, .. } => {
            verify_expression(context, right);
            verify_expression(context, left)
        },
        ValueExpression::Assignment { left, right, .. } => {
            let output = verify_lvalue(context, left)?;

            verify_and_coerce(context, right, &output);

            Some(output)
        },
        ValueExpression::VariableReference { lval_type, .. } => Some(lval_type.clone()),
        ValueExpression::StructFieldReference { field_type, .. } => Some(field_type.clone()),

        _ => {
            println!("Unimplemented value expression {:?}", expr);
            None
        },
    }
}

fn characterize_unverified_expr(context: &mut VerifyContext, expr: &mut UnverifiedExpression) -> Option<Expression> {
    match expr {
        UnverifiedExpression::Identifier(name) => {
            let Some(type_) = context.get_variable(name.as_ref()).cloned() else {
                println!("Variable {} not found", name);
                return None;
            };

            Some(
                Expression::Value(
                    ValueExpression::VariableReference {
                        name: name.clone(),
                        lval_type: type_.clone()
                    }
                )
            )
        },

        UnverifiedExpression::CompoundExpression { prefix, suffix } => {
            let (type_, name) = verify_compound_pair(context, prefix, suffix)?;

            context.insert_variable(name.clone(), type_.clone());

            Some(
                Expression::LValue(
                    LValueExpression::Alloca {
                        type_,
                        name
                    }
                )
            )
        },

        UnverifiedExpression::FunctionCall {
            name, args
        } => {
            match name.as_ref() {
                Expression::Unverified(
                    UnverifiedExpression::Identifier(name)
                ) => {
                    let Some(_) = context.function_table.get(name) else {
                        println!("Function {} not found", name);
                        return None;
                    };

                    Some(
                        Expression::Value(
                            ValueExpression::DirectFunctionCall {
                                name: name.clone(),
                                args: args.clone()
                            }
                        )
                    )
                },

                _ => unimplemented!("Function call with non-identifier name")
            }
        },

        UnverifiedExpression::BinaryOperation {
            left, right, operator: OperatorType::PointerAccess
        } => {
            *expr = UnverifiedExpression::BinaryOperation {
                left: Box::new(
                    Expression::Value(
                        ValueExpression::UnaryOperation {
                            operator: OperatorType::Dereference,
                            operand: Some(left.clone())
                        }
                    )
                ),
                right: right.clone(),
                operator: OperatorType::Access
            };

            characterize_unverified_expr(context, expr)
        },

        UnverifiedExpression::BinaryOperation {
            left, right, operator: OperatorType::Access,
        } => {
            let _type = verify_lvalue(context, left)?;
            let ValueType::Structured { fields } = _type.clone() else {
                println!("Cannot access fields of non-struct type: {:?}", _type);
                return None;
            };
            let Expression::Unverified(UnverifiedExpression::Identifier(right)) = right.as_ref() else {
                println!("Field access requires an identifier, found: {:?}", right);
                return None;
            };

            let Some(field) = fields.iter().find(|(name, _)| name == right) else {
                println!("Field {} not found in struct", right);
                return None;
            };

            Some(
                Expression::Value(
                    ValueExpression::StructFieldReference {
                        struct_: left.to_owned(),
                        field_offset: get_struct_field_offset(context, _type, right)?,
                        field_type: field.1.clone()
                    }
                )
            )
        }

        UnverifiedExpression::BinaryOperation {
            left, right, operator
        } => {
            verify_expression(context, left)?;
            verify_expression(context, right)?;

            Some(
                Expression::Value(
                    ValueExpression::BinaryOperation {
                        left: left.to_owned(),
                        right: right.to_owned(),
                        operator: operator.clone()
                    }
                )
            )
        }

        _ => {
            println!("Unimplemented unverified expression {:?}", expr);
            None
        }
    }
}

fn verify_and_coerce(context: &mut VerifyContext, expr: &mut Expression, target_type: &ValueType) -> Option<()> {
    let current_type = verify_expression(context, expr)?;

    if current_type == *target_type {
        return Some(())
    }

    if let ValueType::Integer { bytes: current_bytes, .. } = current_type {
        if let ValueType::Integer { bytes: target_bytes, .. } = target_type {
            let cast_type = if current_bytes > *target_bytes {
                IntegerCastType::IReduce
            } else {
                IntegerCastType::ZeroExtend
            };

            *expr = Expression::Value(
                ValueExpression::IntegerCast {
                    expr: Box::new(expr.clone()),
                    type_: target_type.clone(),
                    cast_type
                }
            );
            return Some(())
        }
    }

    if let ValueType::Float { bytes: current_bytes } = current_type {
        if let ValueType::Float { bytes: target_bytes } = target_type {
            *expr = Expression::Value(
                ValueExpression::FloatCast {
                    expr: Box::new(expr.clone()),
                    type_: target_type.clone()
                }
            );
            return Some(())
        }
    }

    match expr {
        Expression::Literal(LiteralExpression::IntLiteral { val, .. }) => {
            if let ValueType::Integer { bytes, .. } = target_type {
                *expr = Expression::Literal(
                    LiteralExpression::IntLiteral { val: *val, bytes: *bytes }
                );

                return Some(())
            }
        },

        Expression::Literal(LiteralExpression::FloatLiteral { val, .. }) => {
            if let ValueType::Float { bytes } = target_type {
                *expr = Expression::Literal(
                    LiteralExpression::FloatLiteral { val: *val, bytes: *bytes }
                );

                return Some(())
            }
        },

        _ => ()
    };

    println!("Failed to coerce {:?} to {:?}", expr, target_type);
    None
}