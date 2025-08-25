use cx_data_ast::parse::ast::{CXBinOp, CXCastType, CXExpr, CXExprKind, CXUnOp};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_ast::preparse::pp_type::CX_CONST;
use cx_data_typechecker::ast::{TCExpr, TCExprKind};
use cx_data_typechecker::TCEnvironment;
use cx_util::log_error;
use crate::binary_ops::typecheck_binop;
use crate::casting::{coerce_value, try_implicit_cast};

pub fn typecheck_expr(env: &mut TCEnvironment, expr: &CXExpr) -> Option<TCExpr> {
    Some(
        match &expr.kind {
            CXExprKind::Block { exprs } => {
                let tc_exprs = exprs.iter()
                    .map(|e| typecheck_expr(env, e))
                    .collect::<Option<Vec<_>>>()?;

                TCExpr {
                    _type: CXType::unit(),
                    kind: TCExprKind::Block {
                        statements: tc_exprs,
                    }
                }
            }

            CXExprKind::IntLiteral { val, bytes } => {
                TCExpr {
                    _type: CXType::from(CXTypeKind::Integer { signed: true, bytes: *bytes }),
                    kind: TCExprKind::IntLiteral { value: *val }
                }
            },

            CXExprKind::FloatLiteral { val, bytes } => {
                TCExpr {
                    _type: CXType::from(CXTypeKind::Float { bytes: *bytes }),
                    kind: TCExprKind::FloatLiteral { value: *val }
                }
            },

            CXExprKind::StringLiteral { val } => {
                TCExpr {
                    _type: env.get_type("char")
                        .unwrap()
                        .clone()
                        .add_specifier(CX_CONST),
                    kind: TCExprKind::StringLiteral { value: CXIdent::from(val.as_str()) }
                }
            },

            CXExprKind::VarDeclaration { type_, name } => {
                env.insert_symbol(name.as_string(), type_.clone());

                TCExpr {
                    _type: type_.clone(),
                    kind: TCExprKind::VariableDeclaration { name: name.clone(), type_: type_.clone() }
                }
            },

            CXExprKind::Identifier(name) => {
                if let Some(symbol_type) = env.symbol_type(name.as_str()) {
                    TCExpr {
                        _type: symbol_type.clone(),
                        kind: TCExprKind::VariableIdentifier { name: name.clone() }
                    }
                } else if let Some(function_type) = env.get_func(name.as_str()) {
                    TCExpr {
                        _type: function_type.return_type.clone(),
                        kind: TCExprKind::FunctionIdentifier { name: name.clone() } // Placeholder for args
                    }
                } else {
                    log_error!("Identifier '{}' not found", name);
                }
            },

            CXExprKind::TemplatedIdentifier { .. } => todo!(),

            CXExprKind::If { condition, then_branch, else_branch } => {
                let mut condition_tc = typecheck_expr(env, condition)?;
                coerce_value(&mut condition_tc);

                if !condition_tc._type.is_integer() {
                    let Some(()) = try_implicit_cast(&mut condition_tc, &CXType::from(CXTypeKind::Bool)) else {
                        log_error!("Condition of 'if' must be an integer or convertible to bool");
                    };
                }

                let then_tc = typecheck_expr(env, then_branch)?;
                let else_tc = if let Some(else_branch) = else_branch {
                    Some(typecheck_expr(env, else_branch)?)
                } else {
                    None
                };

                TCExpr {
                    _type: then_tc._type.clone(),
                    kind: TCExprKind::If {
                        condition: Box::new(condition_tc),
                        then_branch: Box::new(then_tc),
                        else_branch: else_tc.map(Box::new)
                    }
                }
            },

            CXExprKind::While { condition, body, pre_eval } => {
                let mut condition_tc = typecheck_expr(env, condition)?;
                coerce_value(&mut condition_tc);

                if !condition_tc._type.is_integer() {
                    let Some(()) = try_implicit_cast(&mut condition_tc, &CXType::from(CXTypeKind::Bool)) else {
                        log_error!("Condition of 'while' must be an integer or convertible to bool");
                    };
                }

                let body_tc = typecheck_expr(env, body)?;

                TCExpr {
                    _type: body_tc._type.clone(),
                    kind: TCExprKind::While {
                        condition: Box::new(condition_tc),
                        body: Box::new(body_tc),
                        pre_eval: *pre_eval
                    }
                }
            },

            CXExprKind::For { init, condition, increment, body } => {
                let init_tc = typecheck_expr(env, init)?;
                let mut condition_tc = typecheck_expr(env, condition)?;
                coerce_value(&mut condition_tc);

                if !condition_tc._type.is_integer() {
                    let Some(()) = try_implicit_cast(&mut condition_tc, &CXType::from(CXTypeKind::Bool)) else {
                        log_error!("Condition of 'for' must be an integer or convertible to bool");
                    };
                }

                let increment_tc = typecheck_expr(env, increment)?;
                let body_tc = typecheck_expr(env, body)?;

                TCExpr {
                    _type: body_tc._type.clone(),
                    kind: TCExprKind::For {
                        init: Box::new(init_tc),
                        condition: Box::new(condition_tc),
                        increment: Box::new(increment_tc),
                        body: Box::new(body_tc)
                    }
                }
            },

            CXExprKind::Break => {
                TCExpr {
                    _type: CXType::from(CXTypeKind::Unit),
                    kind: TCExprKind::Break
                }
            },

            CXExprKind::Continue => {
                TCExpr {
                    _type: CXType::from(CXTypeKind::Unit),
                    kind: TCExprKind::Continue
                }
            },

            CXExprKind::Return { value } => {
                let mut value_tc = if let Some(value) = value {
                    Some(typecheck_expr(env, value)?)
                } else {
                    None
                };

                let return_type = &env.current_function().return_type;

                match (&mut value_tc, return_type) {
                    (Some(value_tc), return_type) if !return_type.is_unit() => {
                        let Some(()) = try_implicit_cast(value_tc, return_type) else {
                            log_error!("TYPE ERROR: Cannot return value of\
                                type {} from function {:?} with return type {}",
                                value_tc._type, env.current_function().name, return_type);
                        };
                    },

                    (None, _) if return_type.is_unit() => {},

                    (Some(_), _) => {
                        log_error!("TYPE ERROR: Cannot return from function {:?} with no return type",
                            env.current_function().name);
                    },

                    (None, _) => {
                        log_error!("TYPE ERROR: Function {:?} expects a return value, but none was provided",
                            env.current_function().name);
                    },
                }

                TCExpr {
                    _type: CXType::from(CXTypeKind::Unit),
                    kind: TCExprKind::Return {
                        value: value_tc.map(Box::new)
                    }
                }
            },

            CXExprKind::Defer { expr } => {
                TCExpr {
                    _type: CXType::from(CXTypeKind::Unit),
                    kind: TCExprKind::Defer {
                        operand: Box::new(typecheck_expr(env, expr)?)
                    }
                }
            },

            CXExprKind::UnOp { operator, operand } => {
                let mut operand_tc = typecheck_expr(env, operand)?;

                match operator {
                    CXUnOp::PreIncrement(_) |
                    CXUnOp::PostIncrement(_) => {
                        let Some(inner) = operand_tc._type.mem_ref_inner() else {
                            log_error!("TYPE ERROR: Cannot apply pre-increment to a non-reference");
                        };

                        if !inner.is_integer() {
                            let Some(_) = try_implicit_cast(&mut operand_tc, &CXType::from(CXTypeKind::Integer { signed: true, bytes: 8 })) else {
                                log_error!("TYPE ERROR: Pre-increment operator requires an integer type");
                            };
                        }

                        TCExpr {
                            _type: operand_tc._type.clone(),
                            kind: TCExprKind::UnOp {
                                operator: operator.clone(),
                                operand: Box::new(operand_tc)
                            }
                        }
                    },

                    CXUnOp::LNot | CXUnOp::BNot | CXUnOp::Negative => {
                        coerce_value(&mut operand_tc);

                        if !operand_tc._type.is_integer() {
                            let Some(_) = try_implicit_cast(&mut operand_tc, &CXType::from(CXTypeKind::Bool)) else {
                                log_error!("TYPE ERROR: Unary operator '{:?}' requires an integer or boolean type", operator);
                            };
                        }

                        TCExpr {
                            _type: operand_tc._type.clone(),
                            kind: TCExprKind::UnOp {
                                operator: operator.clone(),
                                operand: Box::new(operand_tc)
                            }
                        }
                    },

                    CXUnOp::AddressOf => {
                        let Some(inner) = operand_tc._type.mem_ref_inner() else {
                            log_error!("TYPE ERROR: Cannot take address of a non-reference type");
                        };

                        TCExpr {
                            _type: inner.clone().pointer_to(),
                            kind: TCExprKind::UnOp {
                                operator: operator.clone(),
                                operand: Box::new(operand_tc)
                            }
                        }
                    },

                    CXUnOp::Dereference => {
                        let Some(inner) = operand_tc._type.mem_ref_inner().cloned() else {
                            log_error!("TYPE ERROR: Cannot dereference a non-pointer type");
                        };

                        coerce_value(&mut operand_tc);

                        TCExpr {
                            _type: inner.clone(),
                            kind: TCExprKind::UnOp {
                                operator: operator.clone(),
                                operand: Box::new(operand_tc)
                            }
                        }
                    },

                    CXUnOp::ExplicitCast(to_type) => {
                        coerce_value(&mut operand_tc);
                        let to_type = env.contextualize_type(to_type)?;

                        let Some(()) = try_implicit_cast(&mut operand_tc, &to_type) else {
                            log_error!("TYPE ERROR: Cannot cast from {} to {}", operand_tc._type, to_type);
                        };

                        operand_tc
                    }
                }
            },

            CXExprKind::BinOp { op: CXBinOp::Access, lhs, rhs } => {
                let lhs = typecheck_expr(env, lhs)?;

                todo!()
            },

            CXExprKind::BinOp { op, lhs, rhs } => {
                let lhs = typecheck_expr(env, lhs)?;
                let rhs = typecheck_expr(env, rhs)?;

                typecheck_binop(op.clone(), lhs, rhs)?
            },

            CXExprKind::Move { expr } => {
                let mut expr_tc = typecheck_expr(env, expr)?;
                coerce_value(&mut expr_tc);

                if !expr_tc._type.is_strong_pointer() {
                    log_error!("TYPE ERROR: Move expression requires a strong pointer type, found {}", expr_tc._type);
                }

                TCExpr {
                    _type: expr_tc._type.clone(),
                    kind: TCExprKind::Move {
                        operand: Box::new(expr_tc)
                    }
                }
            },

            CXExprKind::New { _type, array_length } => {
                let len_tc = if let Some(len) = array_length {
                    let mut len_tc = typecheck_expr(env, len)?;
                    coerce_value(&mut len_tc);

                    if !len_tc._type.is_integer() {
                        let Some(()) = try_implicit_cast(&mut len_tc, &CXType::from(CXTypeKind::Integer { signed: true, bytes: 8 })) else {
                            log_error!("TYPE ERROR: Array length must be an integer or convertible to integer");
                        };
                    }

                    Some(Box::new(len_tc))
                } else { None };

                TCExpr {
                    _type: CXType::from(
                        CXTypeKind::StrongPointer {
                            inner_type: Box::new(_type.clone()),
                            is_array: len_tc.is_some(),
                        }
                    ),
                    kind: TCExprKind::New {
                        _type: _type.clone(),
                        array_length: len_tc,
                    },
                }
            },

            CXExprKind::InitializerList { .. } |
            CXExprKind::Unit => {
                TCExpr {
                    _type: CXType::from(CXTypeKind::Unit),
                    kind: TCExprKind::Unit
                }
            },
            CXExprKind::SizeOf { expr } => {
                let mut tc_expr = typecheck_expr(env, expr)?;
                coerce_value(&mut tc_expr);

                TCExpr {
                    _type: CXType::from(CXTypeKind::Integer { signed: true, bytes: 8 }),
                    kind: TCExprKind::SizeOf { _type: tc_expr._type }
                }
            },

            CXExprKind::Switch { condition, block, cases, default_case } => {
                let mut tc_condition = typecheck_expr(env, condition)?;
                coerce_value(&mut tc_condition);

                let tc_stmts = block.iter()
                    .map(|e| typecheck_expr(env, e))
                    .collect::<Option<Vec<_>>>()?;

                if !tc_condition._type.is_integer() {
                    let Some(_) = try_implicit_cast(&mut tc_condition, &CXType::from(CXTypeKind::Integer { signed: true, bytes: 8 })) else {
                        log_error!("TYPE ERROR: Switch condition must be an integer or convertible to integer");
                    };
                }

                TCExpr {
                    _type: CXType::from(CXTypeKind::Unit),
                    kind: TCExprKind::Switch {
                        condition: Box::new(tc_condition),
                        block: tc_stmts,
                        cases: cases.clone(),
                        default_case: default_case.clone()
                    }
                }
            },

            CXExprKind::GetFunctionAddr { .. } |
            CXExprKind::ImplicitCast { .. } |
            CXExprKind::ImplicitLoad { .. } => {
                todo!("To be removed")
            }

            CXExprKind::Taken => unreachable!("Taken expressions should not be present in the typechecker"),
        }
    )
}