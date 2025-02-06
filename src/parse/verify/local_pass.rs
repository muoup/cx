use std::clone;
use std::ops::Deref;
use log::{log, warn, Level};
use crate::lex::token::OperatorType;
use crate::lex::token::Token::Operator;
use crate::parse::ast::{ControlExpression, Expression, GlobalStatement, LiteralExpression, MemoryExpression, Root, UnverifiedExpression, ValueExpression};
use crate::parse::val_type::ValType;
use crate::parse::verify::context::{FunctionPrototype, VerifyContext};

pub(crate) fn local_pass(context: &mut VerifyContext, root: &mut Root) -> Option<()> {
    for stmt in root.global_stmts.iter_mut() {
        match stmt {
            GlobalStatement::Function {
                return_type, arguments,
                body, ..
            } => {
                let Some(body) = body else { continue; };

                context.current_return_type = Some(return_type.clone());
                context.push_scope();

                for (name, val_type) in arguments {
                    context.insert_variable(name.clone(), val_type.clone());
                }

                for expr in body.iter_mut() {
                    verify_expression(context, expr).expect("Failed to verify expression");
                }

                context.pop_scope();
            }
        }
    }

    Some(())
}

fn verify_expression(context: &mut VerifyContext, expr: &mut Expression) -> Option<()> {
    match expr {
        Expression::Control(control) => verify_control_expr(context, control)?,
        Expression::Memory(memory) => verify_mem_expr(context, memory)?,
        Expression::Value(value) => verify_val_expr(context, value)?,
        Expression::Unverified(unverified) => {
            *expr = characterize_unverified_expr(context, unverified)?;
            verify_expression(context, expr);
        },


        _ => warn!("Unimplemented expression {:?}", expr),
    };

    Some(())
}

fn verify_control_expr(context: &mut VerifyContext, expr: &mut ControlExpression) -> Option<()> {
    match expr {
        ControlExpression::Return(expr) => {
            verify_expression(context, expr);
        },
        ControlExpression::If { condition, then, else_ } => {
            verify_expression(context, condition);

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
        ControlExpression::ForLoop { init, condition, increment, body } => {
            verify_expression(context, init);
            verify_expression(context, condition);
            verify_expression(context, increment);

            context.push_scope();
            for expr in body.iter_mut() {
                verify_expression(context, expr);
            }
            context.pop_scope();
        },
        ControlExpression::Loop { condition, body, .. } => {
            verify_expression(context, condition);
            context.push_scope();
            for expr in body.iter_mut() {
                verify_expression(context, expr);
            }
            context.pop_scope();
        },
        _ => warn!("Unimplemented control expression {:?}", expr),
    };

    Some(())
}

fn verify_mem_expr(context: &mut VerifyContext, expr: &mut MemoryExpression) -> Option<()> {
    match expr {
        MemoryExpression::VariableDeclaration { name, type_ } => {
            context.insert_variable(name.clone(), type_.clone());
        },
        MemoryExpression::VariableReference { name } => {
            if context.get_variable(name).is_none() {
                warn!("Variable {} not found", name);
            }
        },
        MemoryExpression::VariableStorage { name } => {
            if context.get_variable(name).is_none() {
                warn!("Variable {} not found", name);
            }
        },
        _ => warn!("Unimplemented memory expression {:?}", expr),
    };

    Some(())
}

fn verify_val_expr(context: &mut VerifyContext, expr: &mut ValueExpression) -> Option<()> {
    match expr {
        ValueExpression::DirectFunctionCall {
            args, ..
        } => {
            for arg in args.iter_mut() {
                verify_expression(context, arg);
            }
        },
        ValueExpression::UnaryOperation { operand, .. } => {
            verify_expression(context, operand);
        },
        ValueExpression::BinaryOperation { left, right, .. } => {
            verify_expression(context, left);
            verify_expression(context, right);
        },
        ValueExpression::Assignment { left, right, .. } => {
            verify_expression(context, left);
            verify_expression(context, right);

            // Interpret l-value
            match left.as_mut() {
                Expression::Memory(MemoryExpression::VariableReference { name }) => {
                    *(left.as_mut()) = Expression::Memory(MemoryExpression::VariableStorage { name: name.clone() });
                },
                _ => warn!("Invalid left-hand side of assignment")
            }
        },
    };

    Some(())
}

fn characterize_unverified_expr(context: &mut VerifyContext, expr: &mut UnverifiedExpression) -> Option<Expression> {
    match expr {
        UnverifiedExpression::Identifier(name) => {
            let Some(_) = context.get_variable(name.as_ref()) else {
                warn!("Variable {} not found", name);
                return None;
            };

            Some(
                Expression::Memory(
                    MemoryExpression::VariableReference { name: name.clone() }
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
                    context.function_table.get(name)?;

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

fn get_expression_type(context: &VerifyContext, expr: &mut Expression) -> Option<ValType> {
    match expr {
        Expression::Value(expr) => value_expr_type(context, expr),
        Expression::Memory(expr) => memory_expr_type(context, expr),
        Expression::Literal(expr) => literal_expr_type(expr),

        Expression::Control(_) | Expression::Unit => Some(ValType::Unit),

        _ => None
    }
}

fn value_expr_type(context: &VerifyContext, expr: &mut ValueExpression) -> Option<ValType> {
    Some(
        match expr {
            ValueExpression::DirectFunctionCall { name, .. } => {
                let function = context.function_table.get(name)?;
                function.return_type.clone()
            }

            ValueExpression::BinaryOperation { left, .. } =>
                get_expression_type(context, left)?,

            ValueExpression::UnaryOperation { operand, operator } =>
                match operator {
                    OperatorType::Dereference => {
                        if let ValType::Pointer(inner) = get_expression_type(context, operand)? {
                            *inner
                        } else {
                            return None
                        }
                    },
                    OperatorType::AddressOf =>
                        ValType::Pointer(Box::new(get_expression_type(context, operand)?)),

                    _ => return get_expression_type(context, operand)
                }

            _ => return None
        }
    )
}

fn literal_expr_type(expr: &LiteralExpression) -> Option<ValType> {
    match expr {
        LiteralExpression::IntLiteral { bytes, .. } => Some(ValType::Integer { size: *bytes, signed: true }),
        LiteralExpression::FloatLiteral { bytes, .. } => Some(ValType::Float { size: *bytes }),
        LiteralExpression::StringLiteral(_) => Some(ValType::Pointer(Box::new(ValType::Integer { size: 1, signed: false }))),

        _ => None
    }
}

fn memory_expr_type(context: &VerifyContext, expr: &mut MemoryExpression) -> Option<ValType> {
    match expr {
        MemoryExpression::VariableDeclaration { type_, .. } => Some(type_.clone()),
        MemoryExpression::VariableReference { name } => context.get_variable(name).cloned(),
        MemoryExpression::VariableStorage { name } => context.get_variable(name).cloned(),

        _ => None
    }
}

fn attempt_implicit_cast(expr: &mut Expression, target_type: ValType) -> Option<()> {
    match expr {
        Expression::Literal(lit) => {
            let lit_type = literal_expr_type(lit)?;
            if lit_type == target_type {
                return Some(())
            }

            if let LiteralExpression::IntLiteral { val, .. } = lit {
                if let ValType::Integer { size, signed } = target_type {
                    if signed {
                        *lit = LiteralExpression::IntLiteral { val: *val, bytes: size };
                        return Some(())
                    }
                }
            }

            if let LiteralExpression::FloatLiteral { val, .. } = lit {
                if let ValType::Float { size } = target_type {
                    *lit = LiteralExpression::FloatLiteral { val: *val, bytes: size };
                    return Some(())
                }
            }

            None
        },

        _ => None
    }
}