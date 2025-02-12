use std::clone;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use cranelift_module::Module;
use log::{log, warn, Level};
use crate::lex::token::OperatorType;
use crate::lex::token::Token::Operator;
use crate::parse::ast::{ControlExpression, Expression, GlobalStatement, LValueExpression, LiteralExpression, UnverifiedAST, UnverifiedExpression, ValueExpression, ValueType};
use crate::parse::verify::context::{FunctionPrototype, VerifyContext};
use crate::parse::verify::type_verification::verify_type;

pub(crate) fn local_pass(context: &mut VerifyContext, root: &mut UnverifiedAST) -> Option<()> {
    for stmt in root.statements.iter_mut() {
        match stmt {
            GlobalStatement::Function {
                return_type, arguments,
                body, ..
            } => {
                let Some(body) = body else { continue; };

                context.current_return_type = Some(return_type.clone());
                context.push_scope();

                for arg in arguments {
                    verify_expression(context, arg);
                }

                for expr in body.iter_mut() {
                    verify_expression(context, expr).or_else(|| {
                        println!("Failed to verify expression {:?}", expr);
                        None
                    });
                }

                context.pop_scope();
            },

            _ => ()
        }
    }

    Some(())
}
fn verify_expression(context: &mut VerifyContext, expr: &mut Expression) -> Option<ValueType> {
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

        _ => {
            println!("Unimplemented expression {:?}", expr);
            None
        },
    }
}

fn generate_lvalue(context: &mut VerifyContext, expr: &mut Expression, internal_type: ValueType) -> Option<(Option<String>, ValueType)> {
    match expr {
        Expression::Unverified(UnverifiedExpression::Identifier(name)) =>
            Some((Some(name.clone()), ValueType::Standard(internal_type.clone()))),

        _ => None
    }
}

fn verify_lvalue(context: &mut VerifyContext, expr: &mut Expression) -> Option<ValueType> {
    match expr {
        // Variable Declaration
        Expression::Unverified(unverified) => match unverified {
            UnverifiedExpression::TypedExpression { type_: identifier, suffix } => {
                let type_ = verify_type(context, identifier)?;
                let Some((Some(name), lval_type)) = generate_lvalue(context, suffix, type_) else {
                    println!("Failed to generate l-value for typed expression");
                    return None
                };
                let lval_type = ValueType::new(lval_type);

                *expr =
                    Expression::LValue(
                        LValueExpression::Alloca {
                            name,
                            type_: lval_type.clone()
                        }
                    );

                Some(lval_type)
            },

            // Variable reference
            UnverifiedExpression::Identifier(name) => {
                if let Some(val_type) = context.get_variable(name) {
                    *expr = Expression::LValue(
                        LValueExpression::Value {
                            name: name.clone(),
                        }
                    );

                    return Some(val_type.clone())
                }

                if let Some(type_id) = context.get_type(name) {
                    *expr = Expression::LValue(
                        LValueExpression::Value {
                            name: name.clone(),
                        }
                    );

                    return Some(type_id.clone())
                }

                println!("Variable {} not found", name);
                None
            },

            _ => {
                println!("Invalid l-value: {:?}", expr);
                None
            },
        },

        // Pointer to
        Expression::Value(value) => match value {
            ValueExpression::UnaryOperation { operator: OperatorType::Dereference, operand } => {
                None
            },

            _ => {
                println!("Invalid l-value: {:?}", expr);
                None
            },
        },

        _ => {
            println!("Invalid l-value: {:?}", expr);
            None
        },
    }
}

fn verify_control_expr(context: &mut VerifyContext, expr: &mut ControlExpression) -> Option<ValueType> {
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
        _ => println!("Unimplemented control expression {:?}", expr),
    };

    Some(context.get_type("void").unwrap())
}

fn verify_val_expr(context: &mut VerifyContext, expr: &mut ValueExpression) -> Option<ValueType> {
    match expr {
        ValueExpression::DirectFunctionCall {
            args, name
        } => {
            let Some(FunctionPrototype { return_type, args: intended_args }) =
                context.get_function(&name) else {
                println!("Function {} not found", name);
                return None
            };
            let return_type = return_type.clone();

            for (arg, intended_type) in args.iter_mut().zip(intended_args.clone().into_iter()) {
                verify_expression(context, arg)?;
                attempt_implicit_cast(arg, intended_type)?;
            }

            Some(return_type)
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
            verify_expression(context, right)?;

            attempt_implicit_cast(right, output.clone());

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

fn attempt_implicit_cast(expr: &mut Expression, target_type: ValueType) -> Option<()> {
    match expr {
        Expression::Literal(lit) => {
            if let LiteralExpression::IntLiteral { val, .. } = lit {
                if let ValueType::Integer { bytes, signed } = target_type.as_ref() {
                    if *signed {
                        *lit = LiteralExpression::IntLiteral { val: *val, bytes: *bytes };
                        return Some(())
                    }
                }
            }

            if let LiteralExpression::FloatLiteral { val, .. } = lit {
                if let ValueType::Float { bytes } = target_type.as_ref() {
                    *lit = LiteralExpression::FloatLiteral { val: *val, bytes: *bytes };
                    return Some(())
                }
            }

            None
        },

        _ => None
    }
}