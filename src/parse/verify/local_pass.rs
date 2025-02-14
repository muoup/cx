use std::rc::Rc;
use crate::parse::ast::{ControlExpression, Expression, GlobalStatement, LiteralExpression, UnverifiedAST, UnverifiedExpression, UnverifiedGlobalStatement, ValueExpression, ValueType};
use crate::parse::verify::context::{FunctionPrototype, VerifyContext};
use crate::parse::verify::typing::{format_lvalue, verify_type};

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

        _ => return None
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
        Expression::LValue(lvalue) => {
            println!("Attempting to call verify_expression on l-value {:?}", lvalue);
            None
        },
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

fn verify_lvalue(context: &mut VerifyContext, expr: &mut Expression) -> ExprVerifyResult {
    format_lvalue(context, expr)?;

    match expr {


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
            let FunctionPrototype { return_type, args: intended_args } = func.as_ref();

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

        _ => {
            println!("Unimplemented value expression {:?}", expr);
            None
        },
    }
}

fn characterize_unverified_expr(context: &mut VerifyContext, expr: &mut UnverifiedExpression) -> Option<Expression> {
    match expr {
        UnverifiedExpression::Identifier(name) => {
            let Some(type_) = context.get_variable(name.as_ref()) else {
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

        _ => None
    }
}

fn verify_and_coerce(context: &mut VerifyContext, expr: &mut Expression, target_type: &ValueType) -> Option<()> {
    let current_type = verify_expression(context, expr)?;

    if current_type == *target_type {
        return Some(())
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