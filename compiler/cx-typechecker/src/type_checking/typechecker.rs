use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::accumulation::TypecheckResult;
use crate::type_checking::binary_ops::{
    typecheck_access, typecheck_binop, typecheck_binop_mir_vals, typecheck_is,
    typecheck_method_call,
};
use crate::type_checking::casting::{coerce_condition, coerce_value, explicit_cast, implicit_cast};
use crate::type_completion::prototypes::complete_template_args;
use cx_parsing_data::ast::{CXBinOp, CXExpr, CXExprKind, CXGlobalVariable, CXUnOp};
use cx_parsing_data::data::{CX_CONST, CXLinkageMode};
use cx_typechecker_data::mir::expression::{MIRExpression, MIRExpressionKind, MIRUnOp};
use cx_typechecker_data::mir::program::{MIRBaseMappings, MIRGlobalVarKind, MIRGlobalVariable};
use cx_typechecker_data::mir::types::{CXFloatType, CXIntegerType, MIRTypeKind};
use cx_util::identifier::CXIdent;
use cx_util::{CXError, CXResult};

fn anonymous_name_gen() -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("__anon_{id}")
}

use crate::type_checking::r#match::{typecheck_match, typecheck_switch};
use crate::type_checking::structured_initialization::typecheck_initializer_list;
use cx_typechecker_data::mir::types::MIRType;

pub fn typecheck_expr(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    typecheck_expr_inner(env, base_data, expr, expected_type)
}

pub fn typecheck_expr_inner(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    Ok(match &expr.kind {
        CXExprKind::Block { exprs } => {
            let block = exprs
                .iter()
                .map(|e| typecheck_expr(env, base_data, e, None).map(|res| res.expression))
                .collect::<CXResult<Vec<_>>>()?;

            TypecheckResult::expr2(MIRExpression {
                kind: MIRExpressionKind::Block { statements: block },
                _type: MIRType::unit(),
            })
        }

        CXExprKind::IntLiteral { val, bytes } => TypecheckResult::expr2(MIRExpression {
            kind: MIRExpressionKind::IntLiteral(
                *val,
                CXIntegerType::from_bytes(*bytes).unwrap(),
                true,
            ),
            _type: cx_typechecker_data::mir::types::MIRType::from(MIRTypeKind::Integer {
                _type: CXIntegerType::from_bytes(*bytes).unwrap(),
                signed: true,
            }),
        }),

        CXExprKind::FloatLiteral { val, bytes } => TypecheckResult::expr2(MIRExpression {
            kind: MIRExpressionKind::FloatLiteral(*val, CXFloatType::from_bytes(*bytes).unwrap()),
            _type: cx_typechecker_data::mir::types::MIRType::from(MIRTypeKind::Float {
                _type: CXFloatType::from_bytes(*bytes).unwrap(),
            }),
        }),

        CXExprKind::StringLiteral { val } => {
            let anonymous_name = anonymous_name_gen();
            let name_ident = CXIdent::new(anonymous_name.clone());

            env.realized_globals.insert(
                anonymous_name.clone(),
                MIRGlobalVariable {
                    kind: MIRGlobalVarKind::StringLiteral {
                        name: name_ident.clone(),
                        value: val.clone(),
                    },
                    is_mutable: false,
                    linkage: CXLinkageMode::Static,
                },
            );

            let char_type = env
                .get_realized_type("char")
                .unwrap()
                .clone()
                .pointer_to()
                .add_specifier(CX_CONST);

            TypecheckResult::expr2(MIRExpression {
                kind: MIRExpressionKind::Variable(name_ident),
                _type: char_type,
            })
        }

        CXExprKind::VarDeclaration { _type, name } => {
            let _type = env.complete_type(base_data, _type)?;
            let mem_type = _type.clone().mem_ref_to();

            let allocation = MIRExpression {
                kind: MIRExpressionKind::CreateStackVariable {
                    name: Some(name.clone()),
                    _type: _type.clone(),
                },
                _type: mem_type.clone(),
            };

            env.insert_symbol(
                name.as_string(),
                MIRExpression {
                    kind: MIRExpressionKind::Variable(name.clone()),
                    _type: mem_type,
                },
            );

            TypecheckResult::expr2(allocation)
        }

        CXExprKind::Identifier(name) => {
            if let Some(symbol_val) = env.symbol_value(name.as_str()) {
                TypecheckResult::expr2(symbol_val.clone())
            } else if let Ok(function_type) = env.get_standard_function(base_data, expr, name, None)
            {
                TypecheckResult::expr2(MIRExpression {
                    kind: MIRExpressionKind::FunctionReference {
                        implicit_variables: vec![],
                    },
                    _type: MIRType::from(MIRTypeKind::Function {
                        prototype: Box::new(function_type),
                    }),
                })
            } else if let Ok(global) = global_expr(env, base_data, name.as_str()) {
                TypecheckResult::expr2(global)
            } else {
                return log_typecheck_error!(env, expr, "Identifier '{}' not found", name);
            }
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            // These [for now], are only for functions, as templated type identifiers can only appear
            // in CXNaiveType contexts.

            let input = complete_template_args(env, base_data, template_input)?;
            let function = env.get_standard_function(base_data, expr, name, Some(&input))?;

            TypecheckResult::expr2(MIRExpression {
                kind: MIRExpressionKind::FunctionReference {
                    implicit_variables: vec![],
                },
                _type: MIRType::from(MIRTypeKind::Function {
                    prototype: Box::new(function),
                }),
            })
        }

        CXExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            env.push_scope(None, None);

            let condition_result = typecheck_expr(env, base_data, condition, None)
                .and_then(|c| coerce_condition(env, expr, c.into_expression()))?;
            let then_result = typecheck_expr(env, base_data, then_branch, None)?;
            let else_result = if let Some(else_branch) = else_branch {
                Some(typecheck_expr(env, base_data, else_branch, None)?)
            } else {
                None
            };

            env.pop_scope();

            TypecheckResult::expr2(MIRExpression {
                kind: MIRExpressionKind::If {
                    condition: Box::new(condition_result),
                    then_branch: Box::new(then_result.into_expression()),
                    else_branch: else_result.map(|r| Box::new(r.into_expression())),
                },
                _type: cx_typechecker_data::mir::types::MIRType::unit(),
            })
        }

        CXExprKind::While {
            condition,
            body,
            pre_eval,
        } => {
            env.push_scope(None, None);

            let condition_result = typecheck_expr(env, base_data, condition, None)?;
            let body_result = typecheck_expr(env, base_data, body, None)?;

            env.pop_scope();

            TypecheckResult::expr2(MIRExpression {
                kind: MIRExpressionKind::While {
                    condition: Box::new(condition_result.expression),
                    body: Box::new(body_result.expression),
                    pre_eval: *pre_eval,
                },
                _type: cx_typechecker_data::mir::types::MIRType::unit(),
            })
        }

        CXExprKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            env.push_scope(None, None);

            let init_result = typecheck_expr(env, base_data, init, None)?;
            let condition_result = typecheck_expr(env, base_data, condition, None)?;
            let increment_result = typecheck_expr(env, base_data, increment, None)?;
            let body_result = typecheck_expr(env, base_data, body, None)?;

            env.pop_scope();

            TypecheckResult::expr2(MIRExpression {
                kind: MIRExpressionKind::For {
                    init: Box::new(init_result.expression),
                    condition: Box::new(condition_result.expression),
                    increment: Box::new(increment_result.expression),
                    body: Box::new(body_result.expression),
                },
                _type: cx_typechecker_data::mir::types::MIRType::unit(),
            })
        }

        CXExprKind::Break => {
            let Some(_break_to) = env
                .scope_stack
                .iter()
                .rfind(|inner| inner.break_to.is_some())
                .and_then(|s| s.break_to.clone())
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " 'break' used outside of a loop or switch context"
                );
            };

            TypecheckResult::expr2(MIRExpression {
                kind: MIRExpressionKind::Break,
                _type: MIRType::unit(),
            })
        }

        CXExprKind::Continue => {
            let Some(_continue_to) = env
                .scope_stack
                .iter()
                .rfind(|inner| inner.continue_to.is_some())
                .and_then(|s| s.continue_to.clone())
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " 'continue' used outside of a loop context"
                );
            };

            // TODO: Handle scope destructors
            // For now, continue is represented as a Unit expression
            TypecheckResult::expr2(MIRExpression {
                kind: MIRExpressionKind::Continue,
                _type: MIRType::unit(),
            })
        }

        CXExprKind::Return { value } => {
            let return_type = env.current_function().return_type.clone();

            let value_tc = value
                .as_ref()
                .map(|v| typecheck_expr(env, base_data, v, Some(&return_type)))
                .transpose()?;

            let value = match (&value_tc, &return_type) {
                (Some(some_value), return_type) if !return_type.is_unit() => Some(Box::new(
                    implicit_cast(env, expr, some_value.clone().into_expression(), return_type)?,
                )),

                (None, _) if return_type.is_unit() => None,

                (Some(_), _) => {
                    return log_typecheck_error!(
                        env,
                        expr,
                        " Cannot return from function {} with a void return type",
                        env.current_function()
                    );
                }

                (None, _) => {
                    return log_typecheck_error!(
                        env,
                        expr,
                        " Function {} expects a return value, but none was provided",
                        env.current_function()
                    );
                }
            };

            TypecheckResult::expr(MIRType::unit(), MIRExpressionKind::Return { value })
        }

        CXExprKind::Defer { expr: _ } => {
            todo!()
        }

        CXExprKind::UnOp { operator, operand } => {
            match operator {
                CXUnOp::PreIncrement(increment_amount)
                | CXUnOp::PostIncrement(increment_amount) => {
                    let operand_val =
                        typecheck_expr(env, base_data, operand, None)?.into_expression();
                    let operand_type = operand_val.get_type();

                    let Some(inner) = operand_type.mem_ref_inner() else {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Cannot apply pre-increment to a non-reference {}",
                            operand_type
                        );
                    };

                    match &inner.kind {
                        MIRTypeKind::PointerTo { .. } | MIRTypeKind::Integer { .. } => {
                            match operator {
                                CXUnOp::PreIncrement(_) => TypecheckResult::expr(
                                    operand_type.clone(),
                                    MIRExpressionKind::UnaryOperation {
                                        op: MIRUnOp::PreIncrement(*increment_amount),
                                        operand: Box::new(operand_val),
                                    },
                                ),
                                CXUnOp::PostIncrement(_) => TypecheckResult::expr(
                                    inner.clone(),
                                    MIRExpressionKind::UnaryOperation {
                                        op: MIRUnOp::PostIncrement(*increment_amount),
                                        operand: Box::new(operand_val),
                                    },
                                ),
                                _ => unreachable!(),
                            }
                        }

                        _ => {
                            return log_typecheck_error!(
                                env,
                                operand,
                                " Pre-increment operator requires an integer or pointer type, found {}",
                                inner
                            );
                        }
                    }
                }

                CXUnOp::LNot => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)?;
                    let loaded_operand = coerce_value(env, expr, operand_val.into_expression())?;
                    let loaded_operand_type = loaded_operand.get_type();

                    if !loaded_operand_type.is_integer() {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Logical NOT operator requires an integer type, found {}",
                            loaded_operand_type
                        );
                    }

                    TypecheckResult::unary_op(
                        TypecheckResult::expr2(loaded_operand),
                        MIRUnOp::LNOT,
                        MIRTypeKind::Integer {
                            _type: CXIntegerType::I1,
                            signed: false,
                        }
                        .into(),
                    )
                }

                CXUnOp::BNot => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)?;
                    let mut loaded_op_val = coerce_value(env, expr, operand_val.into_expression())?;
                    let loaded_op_type = loaded_op_val.get_type();

                    if !loaded_op_type.is_integer() {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Bitwise NOT operator requires an integer type, found {}",
                            loaded_op_type
                        );
                    }

                    // If the type is I1 (boolean), we must promote to I32 first
                    if let MIRTypeKind::Integer {
                        _type: CXIntegerType::I1,
                        ..
                    } = loaded_op_type.kind
                    {
                        loaded_op_val = implicit_cast(
                            env,
                            expr,
                            loaded_op_val.clone(),
                            &MIRType::from(MIRTypeKind::Integer {
                                _type: CXIntegerType::I32,
                                signed: true,
                            }),
                        )?;
                    }

                    let result_type = loaded_op_val.get_type();

                    TypecheckResult::unary_op(
                        TypecheckResult::expr2(loaded_op_val),
                        MIRUnOp::BNOT,
                        result_type,
                    )
                }

                CXUnOp::Negative => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)?;
                    let loaded_op_val = coerce_value(env, expr, operand_val.into_expression())?;
                    let loaded_op_type = loaded_op_val.get_type();

                    let operator = match &loaded_op_type.kind {
                        MIRTypeKind::Integer { .. } => MIRUnOp::NEG,
                        MIRTypeKind::Float { .. } => MIRUnOp::FNEG,

                        _ => {
                            return log_typecheck_error!(
                                env,
                                operand,
                                " Negation operator requires an integer or float type, found {}",
                                loaded_op_type
                            );
                        }
                    };

                    TypecheckResult::unary_op(
                        TypecheckResult::expr2(loaded_op_val),
                        operator,
                        loaded_op_type,
                    )
                }

                CXUnOp::AddressOf => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)?;
                    let operand_type = operand_val.get_type();
                    let Some(inner) = operand_type.mem_ref_inner() else {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Cannot take address of a non-reference type"
                        );
                    };

                    // AddressOf just returns the operand (which is a reference) as a pointer
                    TypecheckResult::expr2(MIRExpression {
                        kind: operand_val.into_expression().kind,
                        _type: inner.clone().pointer_to(),
                    })
                }

                CXUnOp::Dereference => {
                    // If the operand is a memory reference of a pointer type, we need to load the value first
                    let loaded_operand = typecheck_expr(env, base_data, operand, None)
                        .and_then(|v| coerce_value(env, expr, v.into_expression()))?;
                    let loaded_operand_type = loaded_operand.get_type();

                    let Some(inner) = loaded_operand_type.ptr_inner().cloned() else {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Cannot dereference a non-pointer type {}",
                            loaded_operand_type
                        );
                    };

                    // Dereference returns a memory reference to the inner type
                    TypecheckResult::expr2(MIRExpression {
                        kind: MIRExpressionKind::Typechange(Box::new(loaded_operand)),
                        _type: inner.mem_ref_to(),
                    })
                }

                CXUnOp::ExplicitCast(to_type) => {
                    let to_type = env.complete_type(base_data, to_type)?;
                    let operand_val = typecheck_expr(env, base_data, operand, Some(&to_type))?;

                    TypecheckResult {
                        expression: explicit_cast(
                            env,
                            expr,
                            operand_val.into_expression(),
                            &to_type,
                        )?,
                    }
                }
            }
        }

        CXExprKind::BinOp {
            op: CXBinOp::Assign(op),
            lhs,
            rhs,
        } => {
            let lhs_val = typecheck_expr(env, base_data, lhs, None)?.into_expression();
            let lhs_type = lhs_val.get_type();

            let Some(inner) = lhs_type.mem_ref_inner() else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Cannot assign to non-reference type {}",
                    lhs_type
                );
            };

            let mut rhs_val = typecheck_expr(env, base_data, rhs, Some(inner))?;

            if let Some(op) = op {
                let loaded_lhs = coerce_value(env, expr, lhs_val.clone())?;
                let loaded_rhs = coerce_value(env, expr, rhs_val.into_expression())?;

                rhs_val = typecheck_binop_mir_vals(env, *op.clone(), loaded_lhs, loaded_rhs, expr)?;
            }

            if inner.get_specifier(CX_CONST) {
                return log_typecheck_error!(env, expr, " Cannot assign to a const type");
            }

            let coerced_rhs_val = implicit_cast(env, expr, rhs_val.into_expression(), inner)?;

            TypecheckResult::expr(
                lhs_val.get_type(),
                MIRExpressionKind::MemoryWrite {
                    target: Box::new(lhs_val),
                    value: Box::new(coerced_rhs_val),
                },
            )
        }

        CXExprKind::BinOp {
            op: CXBinOp::Is,
            lhs,
            rhs,
        } => typecheck_is(env, base_data, lhs, rhs, expr)?,

        CXExprKind::BinOp {
            op: CXBinOp::Access,
            lhs,
            rhs,
        } => {
            let lhs = typecheck_expr(env, base_data, lhs, None)?.into_expression();

            typecheck_access(env, base_data, lhs, rhs, expr)?
        }

        CXExprKind::BinOp {
            op: CXBinOp::MethodCall,
            lhs,
            rhs,
        } => typecheck_method_call(env, base_data, lhs, rhs, expr)?,

        CXExprKind::BinOp { op, lhs, rhs } => {
            typecheck_binop(env, base_data, op.clone(), lhs, rhs, expr)?
        }

        CXExprKind::Move {
            expr: inner_expr, ..
        } => {
            let CXExprKind::Identifier(ident) = &inner_expr.kind else {
                return log_typecheck_error!(
                    env,
                    expr,
                    "Move expressions can currently only be applied to stack variable indentifiers"
                );
            };

            let Some(inner_val) = env.symbol_table.get(ident.as_str()) else {
                return log_typecheck_error!(env, expr, " Identifier '{}' not found", ident);
            };

            if !matches!(inner_val.kind, MIRExpressionKind::Variable(_)) {
                return log_typecheck_error!(
                    env,
                    expr,
                    "Move expressions can currently only be applied to stack variable indentifiers"
                );
            }

            let Some(inner_type) = inner_val._type.mem_ref_inner().cloned() else {
                unreachable!()
            };

            TypecheckResult::expr(
                inner_type,
                MIRExpressionKind::Move {
                    source: Box::new(inner_val.clone()),
                },
            )
        }

        CXExprKind::New { _type } => {
            todo!(
                "Intrinsic strong pointers are no longer supported, new semantics for 'new' tbd."
            );
        }

        CXExprKind::InitializerList { indices } => {
            typecheck_initializer_list(env, base_data, expr, indices, expected_type)?
        }

        CXExprKind::TypeConstructor {
            union_name: type_name,
            variant_name: name,
            inner,
        } => {
            let union_type = env.get_type(base_data, type_name.as_str())?;
            let MIRTypeKind::TaggedUnion { variants, .. } = &union_type.kind else {
                return log_typecheck_error!(env, expr, " Unknown type: {}", type_name);
            };

            let Some((i, variant_type)) = variants
                .iter()
                .enumerate()
                .find(|(_, (variant_name, _))| variant_name == name.as_str())
                .map(|(i, (_, variant_type))| (i, variant_type.clone()))
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Variant '{}' not found in tagged union type {}",
                    name,
                    type_name
                );
            };

            let inner = typecheck_expr(env, base_data, inner, Some(&variant_type))
                .and_then(|v| implicit_cast(env, expr, v.into_expression(), &variant_type))?;

            let allocation = TypecheckResult::expr(
                union_type.clone(),
                MIRExpressionKind::CreateStackVariable {
                    name: None,
                    _type: union_type.clone(),
                },
            );

            TypecheckResult::expr(
                union_type.clone().mem_ref_to(),
                MIRExpressionKind::TaggedUnionSet {
                    target: Box::new(allocation.into_expression()),
                    variant_index: i,
                    inner_value: Box::new(inner),
                    sum_type: union_type.clone(),
                },
            )
        }

        CXExprKind::Unit => TypecheckResult::expr2(MIRExpression {
            kind: MIRExpressionKind::Unit,
            _type: cx_typechecker_data::mir::types::MIRType::unit(),
        }),

        CXExprKind::SizeOf { expr } => {
            let tc_expr = typecheck_expr(env, base_data, expr, None)?;
            let tc_type = tc_expr.get_type();

            TypecheckResult::expr2(MIRExpression {
                kind: MIRExpressionKind::IntLiteral(
                    tc_type.type_size() as i64,
                    CXIntegerType::I64,
                    false,
                ),
                _type: cx_typechecker_data::mir::types::MIRType::from(MIRTypeKind::Integer {
                    _type: CXIntegerType::I64,
                    signed: false,
                }),
            })
        }

        CXExprKind::Switch {
            condition,
            block,
            cases,
            default_case,
        } => typecheck_switch(
            env,
            base_data,
            condition,
            block,
            cases,
            default_case.as_ref(),
        )?,

        CXExprKind::Match {
            condition,
            arms,
            default,
        } => typecheck_match(env, base_data, expr, condition, arms, default.as_ref())?,

        CXExprKind::Taken => {
            unreachable!("Taken expressions should not be present in the typechecker")
        }
    })
}

pub(crate) fn global_expr(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    ident: &str,
) -> CXResult<MIRExpression> {
    if let Some(global) = env.realized_globals.get(ident) {
        return tcglobal_expr(global);
    }

    let Some(module_res) = base_data.global_variables.get(ident) else {
        return CXError::create_result(format!("Global variable '{}' not found", ident));
    };

    let module_res = match env.in_external_templated_function {
        true => module_res.clone().transfer(""),
        false => module_res.clone(),
    };

    match &module_res.resource {
        CXGlobalVariable::EnumConstant(val) => Ok(MIRExpression {
            kind: MIRExpressionKind::IntLiteral(
                *val as i64,
                CXIntegerType::from_bytes(8).unwrap(),
                true,
            ),
            _type: MIRType::from(MIRTypeKind::Integer {
                _type: CXIntegerType::from_bytes(8).unwrap(),
                signed: true,
            }),
        }),

        CXGlobalVariable::Standard {
            _type,
            initializer,
            is_mutable,
        } => {
            let _type = env.complete_type(base_data, _type)?;
            let _initializer = match initializer.as_ref() {
                Some(init_expr) => {
                    let CXExprKind::IntLiteral { val, .. } = &init_expr.kind else {
                        return log_typecheck_error!(
                            env,
                            init_expr,
                            " CX currently only supports integer initializers for global variable initialization"
                        );
                    };

                    Some(*val)
                }

                None => None,
            };

            env.realized_globals.insert(
                ident.to_string(),
                MIRGlobalVariable {
                    kind: MIRGlobalVarKind::Variable {
                        name: CXIdent::new(ident.to_string()),
                        initializer: _initializer,
                        _type,
                    },
                    is_mutable: *is_mutable,
                    linkage: module_res.linkage,
                },
            );

            tcglobal_expr(env.realized_globals.get(ident).unwrap())
        }
    }
}

fn tcglobal_expr(global: &MIRGlobalVariable) -> CXResult<MIRExpression> {
    match &global.kind {
        MIRGlobalVarKind::Variable { name, _type, .. } => Ok(MIRExpression {
            kind: MIRExpressionKind::Variable(name.clone()),
            _type: _type.clone().mem_ref_to(),
        }),

        MIRGlobalVarKind::StringLiteral { .. } => {
            unreachable!("String literals cannot be referenced via an identifier")
        }
    }
}
