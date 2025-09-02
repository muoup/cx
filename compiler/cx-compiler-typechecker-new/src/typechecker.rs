use cx_data_ast::parse::ast::{CXBinOp, CXCastType, CXExpr, CXExprKind, CXUnOp};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_typechecker::cx_types::{CXFunctionPrototype, CXType, CXTypeKind};
use cx_data_ast::preparse::naive_types::CX_CONST;
use cx_data_typechecker::ast::{TCExpr, TCExprKind, TCInitIndex};
use cx_data_typechecker::TCEnvironment;
use cx_util::log_error;
use crate::binary_ops::{typecheck_access, typecheck_binop, typecheck_method_call};
use crate::casting::{coerce_condition, coerce_value, explicit_cast, implicit_cast, try_implicit_cast};
use crate::realize_fn_prototype;
use crate::templates::instantiate_function_template;
use crate::type_mapping::{contextualize_template_args, contextualize_type};

pub(crate) fn setup_method_env(env: &mut TCEnvironment, prototype: &CXFunctionPrototype) {
    env.push_scope();

    for param in prototype.params.iter() {
        if let Some(name) = &param.name {
            env.insert_symbol(name.as_string(), param._type.clone());
        }
    }

    env.current_function = Some(prototype.clone());
}

pub(crate) fn cleanup_method_env(env: &mut TCEnvironment) {
    env.current_function = None;
    env.pop_scope();
}

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
                        .pointer_to()
                        .add_specifier(CX_CONST),
                    kind: TCExprKind::StringLiteral { value: CXIdent::from(val.as_str()) }
                }
            },

            CXExprKind::VarDeclaration { type_, name } => {
                let type_ = contextualize_type(env, &type_)?;

                env.insert_symbol(name.as_string(), type_.clone());

                TCExpr {
                    _type: type_.clone().mem_ref_to(),
                    kind: TCExprKind::VariableDeclaration { name: name.clone(), type_: type_.clone() }
                }
            },

            CXExprKind::Identifier(name) => {
                if let Some(symbol_type) = env.symbol_type(name.as_str()) {
                    TCExpr {
                        _type: symbol_type.clone().mem_ref_to(),
                        kind: TCExprKind::VariableReference { name: name.clone() }
                    }
                } else if let Some(function_type) = env.get_func(name.as_str()) {
                    TCExpr {
                        _type: CXTypeKind::Function { prototype: Box::new(function_type.clone()) }.into(),
                        kind: TCExprKind::FunctionReference { name: name.clone() } // Placeholder for args
                    }
                } else {
                    log_error!("Identifier '{}' not found", name);
                }
            },

            CXExprKind::TemplatedIdentifier { name, template_input } => {
                // These [for now], are only for functions, as templated type identifiers can only appear
                // in CXNaiveType contexts.

                let input = contextualize_template_args(env, template_input)?;
                let function = instantiate_function_template(env, name.as_str(), &input)?;

                TCExpr {
                    _type: CXTypeKind::Function { prototype: Box::new(function.clone()) }.into(),
                    kind: TCExprKind::FunctionReference { name: function.name.clone() }
                }
            },

            CXExprKind::If { condition, then_branch, else_branch } => {
                let mut condition_tc = typecheck_expr(env, condition)?;
                coerce_condition(&mut condition_tc);

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
                coerce_condition(&mut condition_tc);

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
                coerce_condition(&mut condition_tc);

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
                    let mut val = typecheck_expr(env, value)?;
                    coerce_value(&mut val);

                    Some(val)
                } else {
                    None
                };

                let return_type = &env.current_function().return_type;

                match (&mut value_tc, return_type) {
                    (Some(value_tc), return_type) if !return_type.is_unit() => {
                        implicit_cast(value_tc, return_type);
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
                            log_error!("TYPE ERROR: Cannot apply pre-increment to a non-reference {}", operand_tc._type);
                        };

                        match &inner.kind {
                            CXTypeKind::Integer { .. } |
                            CXTypeKind::PointerTo { .. } => (),

                            _ => log_error!("TYPE ERROR: Pre-increment operator requires an integer or pointer type, found {}", inner),
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
                            implicit_cast(&mut operand_tc, &CXType::from(CXTypeKind::Integer { signed: true, bytes: 8 }));
                        }

                        let return_type = match operator {
                            CXUnOp::LNot => CXType::from(CXTypeKind::Bool),

                            _ => operand_tc._type.clone()
                        };

                        TCExpr {
                            _type: return_type,
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
                        coerce_value(&mut operand_tc);

                        let Some(inner) = operand_tc._type.ptr_inner().cloned() else {
                            log_error!("TYPE ERROR: Cannot dereference a non-pointer type {}", operand_tc._type);
                        };

                        coerce_value(&mut operand_tc);

                        TCExpr {
                            _type: inner.mem_ref_to(),
                            kind: TCExprKind::UnOp {
                                operator: operator.clone(),
                                operand: Box::new(operand_tc)
                            }
                        }
                    },

                    CXUnOp::ExplicitCast(to_type) => {
                        coerce_value(&mut operand_tc);
                        let to_type = contextualize_type(env, to_type)?;
                        explicit_cast(&mut operand_tc, &to_type);

                        operand_tc
                    }
                }
            },

            CXExprKind::BinOp { op: CXBinOp::Assign(op), lhs, rhs } => {
                let lhs = typecheck_expr(env, lhs)?;
                let mut rhs = typecheck_expr(env, rhs)?;

                if lhs._type.get_specifier(CX_CONST) {
                    log_error!("TYPE ERROR: Assignment operator cannot be applied to const variables");
                }

                coerce_value(&mut rhs);

                let Some(inner) = lhs._type.mem_ref_inner() else {
                    log_error!("TYPE ERROR: Cannot assign to a non-reference type {}", lhs._type);
                };

                if inner.is_structured() {
                    implicit_cast(&mut rhs, inner);
                }

                TCExpr {
                    _type: lhs._type.clone(),
                    kind: TCExprKind::Assignment {
                        target: Box::new(lhs),
                        value: Box::new(rhs),
                        additional_op: match op {
                            Some(op) => Some(*op.clone()),
                            None => None
                        },
                    }
                }
            },

            CXExprKind::BinOp { op: CXBinOp::Access, lhs, rhs } =>
                typecheck_access(env, lhs, rhs)?,

            CXExprKind::BinOp { op: CXBinOp::MethodCall, lhs, rhs } =>
                typecheck_method_call(env, lhs, rhs)?,

            CXExprKind::BinOp { op, lhs, rhs } => {
                let lhs = typecheck_expr(env, lhs)?;
                let rhs = typecheck_expr(env, rhs)?;

                typecheck_binop(op.clone(), lhs, rhs)?
            },

            CXExprKind::Move { expr } => {
                let mut expr_tc = typecheck_expr(env, expr)?;

                let Some(inner) = expr_tc._type.mem_ref_inner() else {
                    log_error!("TYPE ERROR: Move expression requires a reference type, found {}", expr_tc._type);
                };

                if !inner.is_strong_pointer() {
                    log_error!("TYPE ERROR: Move expression requires a strong pointer type, found {}", expr_tc._type);
                }

                TCExpr {
                    _type: expr_tc._type.clone(),
                    kind: TCExprKind::Move {
                        operand: Box::new(expr_tc)
                    }
                }
            },

            CXExprKind::New { _type } => {
                let _type = contextualize_type(env, _type)?;

                TCExpr {
                    _type: CXType::from(
                        CXTypeKind::StrongPointer {
                            inner_type: Box::new(_type.clone()),
                            is_array: false,
                        }
                    ),
                    kind: TCExprKind::New {
                        _type: _type.clone(),
                        array_length: None,
                    },
                }
            },

            CXExprKind::InitializerList { indices } => {
                let mut tc_indices = Vec::new();

                for index in indices.iter() {
                    let mut tc_expr = typecheck_expr(env, &index.value)?;
                    coerce_value(&mut tc_expr);

                    tc_indices.push(TCInitIndex {
                        name: index.name.clone(),
                        index: index.index,
                        value: tc_expr,
                    });
                }

                TCExpr {
                    _type: CXType::from(CXTypeKind::Unit), // Placeholder, will be set during assignment
                    kind: TCExprKind::InitializerList {
                        indices: tc_indices
                    }
                }
            },

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
                coerce_condition(&mut tc_condition);

                let tc_stmts = block.iter()
                    .map(|e| typecheck_expr(env, e))
                    .collect::<Option<Vec<_>>>()?;

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

            CXExprKind::Taken => unreachable!("Taken expressions should not be present in the typechecker"),
        }
    )
}