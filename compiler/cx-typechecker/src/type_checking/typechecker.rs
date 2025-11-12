use crate::environment::TCEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::binary_ops::{
    typecheck_access, typecheck_binop, typecheck_is, typecheck_method_call,
};
use crate::type_checking::casting::{coerce_condition, coerce_value, explicit_cast, implicit_cast};
use crate::type_checking::move_semantics::acknowledge_declared_type;
use crate::type_completion::prototypes::complete_template_args;
use cx_parsing_data::ast::{CXBinOp, CXExpr, CXExprKind, CXGlobalVariable, CXUnOp};
use cx_parsing_data::data::{CX_CONST, CXLinkageMode, NaiveFnIdent, NaiveFnKind};
use cx_typechecker_data::ast::{
    TCBaseMappings, TCExpr, TCExprKind, TCGlobalVarKind, TCGlobalVariable, TCInitIndex, TCTagMatch,
};
use cx_typechecker_data::cx_types::{CXType, CXTypeKind, TCFunctionPrototype};
use cx_util::identifier::CXIdent;
use cx_util::{CXError, CXResult};

fn anonymous_name_gen() -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("__anon_{id}")
}

pub(crate) fn in_method_env(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    prototype: &TCFunctionPrototype,
    expr: &CXExpr,
) -> CXResult<TCExpr> {
    setup_method_env(env, prototype);
    let tc_expr = typecheck_expr(env, base_data, expr)?;
    cleanup_method_env(env);

    Ok(tc_expr)
}

pub(crate) fn setup_method_env(env: &mut TCEnvironment, prototype: &TCFunctionPrototype) {
    env.push_scope();

    for param in prototype.params.iter() {
        if let Some(name) = &param.name {
            env.insert_symbol(name.as_string(), param._type.clone().mem_ref_to());
        }
    }

    env.current_function = Some(prototype.clone());
}

pub(crate) fn cleanup_method_env(env: &mut TCEnvironment) {
    env.current_function = None;
    env.pop_scope();
}

pub fn typecheck_expr(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    expr: &CXExpr,
) -> CXResult<TCExpr> {
    Ok(match &expr.kind {
        CXExprKind::Block { exprs } => {
            env.push_scope();

            let tc_exprs = exprs
                .iter()
                .map(|e| typecheck_expr(env, base_data, e))
                .collect::<CXResult<Vec<_>>>()?;

            env.pop_scope();

            TCExpr {
                _type: CXType::unit(),
                kind: TCExprKind::Block {
                    statements: tc_exprs,
                },
            }
        }

        CXExprKind::IntLiteral { val, bytes } => TCExpr {
            _type: CXType::from(CXTypeKind::Integer {
                signed: true,
                bytes: *bytes,
            }),
            kind: TCExprKind::IntLiteral { value: *val },
        },

        CXExprKind::FloatLiteral { val, bytes } => TCExpr {
            _type: CXType::from(CXTypeKind::Float { bytes: *bytes }),
            kind: TCExprKind::FloatLiteral {
                value: val.into(),
            },
        },

        CXExprKind::StringLiteral { val } => {
            let anonymous_name = anonymous_name_gen();
            let name_ident = CXIdent::from(anonymous_name.clone());

            env.realized_globals.insert(
                anonymous_name.clone(),
                TCGlobalVariable {
                    kind: TCGlobalVarKind::StringLiteral {
                        name: name_ident.clone(),
                        value: val.clone(),
                    },
                    is_mutable: false,
                    linkage: CXLinkageMode::Static,
                },
            );

            TCExpr {
                _type: env
                    .get_realized_type("char")
                    .unwrap()
                    .clone()
                    .pointer_to()
                    .add_specifier(CX_CONST),
                kind: TCExprKind::GlobalVariableReference { name: name_ident },
            }
        }

        CXExprKind::VarDeclaration { type_, name } => {
            let type_ = env.complete_type(base_data, type_)?;

            env.insert_symbol(name.as_string(), type_.clone().mem_ref_to());
            acknowledge_declared_type(env, base_data, &type_);

            TCExpr {
                _type: type_.clone().mem_ref_to(),
                kind: TCExprKind::VariableDeclaration {
                    name: name.clone(),
                    type_,
                },
            }
        }

        CXExprKind::Identifier(name) => {
            if let Some(symbol_type) = env.symbol_type(name.as_str()) {
                TCExpr {
                    _type: symbol_type.clone(),
                    kind: TCExprKind::VariableReference { name: name.clone() },
                }
            } else if let Some(function_type) = env
                .get_func(base_data, &NaiveFnIdent::Standard(name.clone()))
                .ok()
            {
                TCExpr {
                    _type: CXTypeKind::Function {
                        prototype: Box::new(function_type.clone()),
                    }
                    .into(),
                    kind: TCExprKind::FunctionReference,
                }
            } else if let Some(global) = global_expr(env, base_data, name.as_str()).ok() {
                global
            } else {
                log_typecheck_error!(env, expr, "Identifier '{}' not found", name);
            }
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            // These [for now], are only for functions, as templated type identifiers can only appear
            // in CXNaiveType contexts.

            let input = complete_template_args(env, base_data, template_input)?;
            let ident = NaiveFnKind::Standard(name.clone());
            
            let function = env.get_func_templated(base_data, &ident, &input)?;

            TCExpr {
                _type: CXTypeKind::Function {
                    prototype: Box::new(function.clone()),
                }
                .into(),
                kind: TCExprKind::FunctionReference,
            }
        }

        CXExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            env.push_scope();

            let mut condition_tc = typecheck_expr(env, base_data, condition)?;
            coerce_condition(&mut condition_tc)?;

            let then_tc = typecheck_expr(env, base_data, then_branch)?;
            let else_tc = if let Some(else_branch) = else_branch {
                Some(typecheck_expr(env, base_data, else_branch)?)
            } else {
                None
            };

            env.pop_scope();

            TCExpr {
                _type: then_tc._type.clone(),
                kind: TCExprKind::If {
                    condition: Box::new(condition_tc),
                    then_branch: Box::new(then_tc),
                    else_branch: else_tc.map(Box::new),
                },
            }
        }

        CXExprKind::While {
            condition,
            body,
            pre_eval,
        } => {
            env.push_scope();

            let mut condition_tc = typecheck_expr(env, base_data, condition)?;
            coerce_condition(&mut condition_tc)?;

            let body_tc = typecheck_expr(env, base_data, body)?;

            env.pop_scope();

            TCExpr {
                _type: body_tc._type.clone(),
                kind: TCExprKind::While {
                    condition: Box::new(condition_tc),
                    body: Box::new(body_tc),
                    pre_eval: *pre_eval,
                },
            }
        }

        CXExprKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            env.push_scope();

            let init_tc = typecheck_expr(env, base_data, init)?;
            let mut condition_tc = typecheck_expr(env, base_data, condition)?;
            coerce_condition(&mut condition_tc)?;

            let increment_tc = typecheck_expr(env, base_data, increment)?;
            let body_tc = typecheck_expr(env, base_data, body)?;

            env.pop_scope();

            TCExpr {
                _type: body_tc._type.clone(),
                kind: TCExprKind::For {
                    init: Box::new(init_tc),
                    condition: Box::new(condition_tc),
                    increment: Box::new(increment_tc),
                    body: Box::new(body_tc),
                },
            }
        }

        CXExprKind::Break => TCExpr {
            _type: CXType::from(CXTypeKind::Unit),
            kind: TCExprKind::Break,
        },

        CXExprKind::Continue => TCExpr {
            _type: CXType::from(CXTypeKind::Unit),
            kind: TCExprKind::Continue,
        },

        CXExprKind::Return { value } => {
            let mut value_tc = if let Some(value) = value {
                let mut val = typecheck_expr(env, base_data, value)?;
                coerce_value(&mut val)?;

                Some(val)
            } else {
                None
            };

            let return_type = &env.current_function().return_type;

            match (&mut value_tc, return_type) {
                (Some(value_tc), return_type) if !return_type.is_unit() => {
                    implicit_cast(value_tc, return_type)?;
                }

                (None, _) if return_type.is_unit() => {}

                (Some(_), _) => {
                    log_typecheck_error!(
                        env,
                        expr,
                        " Cannot return from function {} with a void return type",
                        env.current_function()
                    );
                }

                (None, _) => {
                    log_typecheck_error!(
                        env,
                        expr,
                        " Function {} expects a return value, but none was provided",
                        env.current_function()
                    );
                }
            }

            TCExpr {
                _type: CXType::from(CXTypeKind::Unit),
                kind: TCExprKind::Return {
                    value: value_tc.map(Box::new),
                },
            }
        }

        CXExprKind::Defer { expr } => TCExpr {
            _type: CXType::from(CXTypeKind::Unit),
            kind: TCExprKind::Defer {
                operand: Box::new(typecheck_expr(env, base_data, expr)?),
            },
        },

        CXExprKind::UnOp { operator, operand } => {
            let mut operand_tc = typecheck_expr(env, base_data, operand)?;

            match operator {
                CXUnOp::PreIncrement(_) | CXUnOp::PostIncrement(_) => {
                    let Some(inner) = operand_tc._type.mem_ref_inner() else {
                        log_typecheck_error!(
                            env,
                            operand,
                            " Cannot apply pre-increment to a non-reference {}",
                            operand_tc._type
                        );
                    };

                    match &inner.kind {
                        CXTypeKind::Integer { .. } | CXTypeKind::PointerTo { .. } => (),

                        _ => log_typecheck_error!(
                            env,
                            operand,
                            " Pre-increment operator requires an integer or pointer type, found {}",
                            inner
                        ),
                    }

                    TCExpr {
                        _type: operand_tc._type.clone(),
                        kind: TCExprKind::UnOp {
                            operator: operator.clone(),
                            operand: Box::new(operand_tc),
                        },
                    }
                }

                CXUnOp::LNot | CXUnOp::BNot | CXUnOp::Negative => {
                    coerce_value(&mut operand_tc)?;

                    if !operand_tc._type.is_integer() {
                        implicit_cast(
                            &mut operand_tc,
                            &CXType::from(CXTypeKind::Integer {
                                signed: true,
                                bytes: 8,
                            }),
                        )?;
                    }

                    let return_type = match operator {
                        CXUnOp::LNot => CXType::from(CXTypeKind::Bool),

                        _ => operand_tc._type.clone(),
                    };

                    TCExpr {
                        _type: return_type,
                        kind: TCExprKind::UnOp {
                            operator: operator.clone(),
                            operand: Box::new(operand_tc),
                        },
                    }
                }

                CXUnOp::AddressOf => {
                    let Some(inner) = operand_tc._type.mem_ref_inner() else {
                        log_typecheck_error!(
                            env,
                            operand,
                            " Cannot take address of a non-reference type"
                        );
                    };

                    TCExpr {
                        _type: inner.clone().pointer_to(),
                        kind: TCExprKind::UnOp {
                            operator: operator.clone(),
                            operand: Box::new(operand_tc),
                        },
                    }
                }

                CXUnOp::Dereference => {
                    coerce_value(&mut operand_tc)?;

                    let Some(inner) = operand_tc._type.ptr_inner().cloned() else {
                        log_typecheck_error!(
                            env,
                            operand,
                            " Cannot dereference a non-pointer type {}",
                            operand_tc._type
                        );
                    };

                    coerce_value(&mut operand_tc)?;

                    TCExpr {
                        _type: inner.mem_ref_to(),
                        kind: TCExprKind::UnOp {
                            operator: operator.clone(),
                            operand: Box::new(operand_tc),
                        },
                    }
                }

                CXUnOp::ExplicitCast(to_type) => {
                    coerce_value(&mut operand_tc)?;
                    let to_type = env.complete_type(base_data, to_type)?;
                    explicit_cast(&mut operand_tc, &to_type)?;

                    operand_tc
                }
            }
        }

        CXExprKind::BinOp {
            op: CXBinOp::Assign(op),
            lhs,
            rhs,
        } => {
            let lhs = typecheck_expr(env, base_data, lhs)?;
            let mut rhs = typecheck_expr(env, base_data, rhs)?;

            let Some(inner) = lhs._type.mem_ref_inner() else {
                log_typecheck_error!(
                    env,
                    expr,
                    " Cannot assign to non-reference type {}",
                    lhs._type
                );
            };

            if inner.get_specifier(CX_CONST)
                && !matches!(lhs.kind, TCExprKind::VariableDeclaration { .. })
            {
                log_typecheck_error!(env, expr, " Cannot assign to a const type");
            }

            implicit_cast(&mut rhs, inner)?;

            TCExpr {
                _type: lhs._type.clone(),
                kind: TCExprKind::Assignment {
                    target: Box::new(lhs),
                    value: Box::new(rhs),
                    additional_op: op.as_ref().map(|op| *op.clone()),
                },
            }
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
        } => typecheck_access(env, base_data, lhs, rhs, expr)?,

        CXExprKind::BinOp {
            op: CXBinOp::MethodCall,
            lhs,
            rhs,
        } => typecheck_method_call(env, base_data, lhs, rhs, expr)?,

        CXExprKind::BinOp { op, lhs, rhs } => {
            let lhs = typecheck_expr(env, base_data, lhs)?;
            let rhs = typecheck_expr(env, base_data, rhs)?;

            typecheck_binop(env, op.clone(), lhs, rhs, expr)?
        }

        CXExprKind::Move { expr: move_expr } => {
            let expr_tc = typecheck_expr(env, base_data, move_expr)?;

            let Some(inner) = expr_tc._type.mem_ref_inner() else {
                log_typecheck_error!(
                    env,
                    move_expr,
                    " Move expression requires a reference type, found {}",
                    expr_tc._type
                );
            };

            if !inner.has_move_semantics() {
                log_typecheck_error!(
                    env,
                    move_expr,
                    "Value of type {} has no move semantics",
                    inner
                );
            }

            TCExpr {
                _type: inner.clone(),
                kind: TCExprKind::Move {
                    operand: Box::new(expr_tc),
                },
            }
        }

        CXExprKind::New { _type } => {
            let mut _type = env.complete_type(base_data, _type)?;

            let (_type, array_length) = match _type.kind {
                CXTypeKind::Array {
                    inner_type, size, ..
                } => {
                    let length = TCExpr {
                        _type: CXType::from(CXTypeKind::Integer {
                            signed: true,
                            bytes: 8,
                        }),
                        kind: TCExprKind::IntLiteral { value: size as i64 },
                    };

                    (*inner_type, Some(Box::new(length)))
                }

                CXTypeKind::VariableLengthArray {
                    _type: inner_type,
                    size,
                } => {
                    let mut size = *size;
                    coerce_value(&mut size)?;

                    _type = inner_type.clone().pointer_to();
                    (*inner_type, Some(Box::new(size)))
                }

                _ => (_type, None),
            };

            TCExpr {
                _type: CXType::from(CXTypeKind::StrongPointer {
                    inner_type: Box::new(_type.clone()),
                    is_array: array_length.is_some(),
                }),
                kind: TCExprKind::New {
                    _type: _type.clone(),
                    array_length,
                },
            }
        }

        CXExprKind::InitializerList { indices } => {
            let mut tc_indices = Vec::new();

            for index in indices.iter() {
                let mut tc_expr = typecheck_expr(env, base_data, &index.value)?;
                coerce_value(&mut tc_expr)?;

                tc_indices.push(TCInitIndex {
                    name: index.name.clone(),
                    index: index.index,
                    value: tc_expr,
                });
            }

            TCExpr {
                _type: CXType::from(CXTypeKind::Unit), // Placeholder, will be set during assignment
                kind: TCExprKind::InitializerList {
                    indices: tc_indices,
                },
            }
        }

        CXExprKind::TypeConstructor {
            union_name: type_name,
            variant_name: name,
            inner,
        } => {
            let union_type = env.get_type(base_data, type_name.as_str())?;
            let CXTypeKind::TaggedUnion { variants, .. } = &union_type.kind else {
                log_typecheck_error!(env, expr, " Unknown type: {}", type_name);
            };

            let Some((i, variant_type)) = variants
                .iter()
                .enumerate()
                .find(|(_, (variant_name, _))| variant_name == name.as_str())
                .map(|(i, (_, variant_type))| (i, variant_type.clone()))
            else {
                log_typecheck_error!(
                    env,
                    expr,
                    " Variant '{}' not found in tagged union type {}",
                    name,
                    type_name
                );
            };

            let mut inner = typecheck_expr(env, base_data, inner)?;
            implicit_cast(&mut inner, &variant_type)?;

            TCExpr {
                _type: union_type.clone().mem_ref_to(),
                kind: TCExprKind::TypeConstructor {
                    name: name.clone(),

                    union_type,
                    variant_type,
                    variant_index: i,

                    input: Box::new(inner),
                },
            }
        }

        CXExprKind::Unit => TCExpr {
            _type: CXType::from(CXTypeKind::Unit),
            kind: TCExprKind::Unit,
        },

        CXExprKind::SizeOf { expr } => {
            let tc_expr = typecheck_expr(env, base_data, expr)?;

            TCExpr {
                _type: CXType::from(CXTypeKind::Integer {
                    signed: true,
                    bytes: 8,
                }),
                kind: TCExprKind::SizeOf {
                    _type: tc_expr._type,
                },
            }
        }

        CXExprKind::Switch {
            condition,
            block,
            cases,
            default_case,
        } => {
            let mut tc_condition = typecheck_expr(env, base_data, condition)?;
            coerce_condition(&mut tc_condition)?;

            let tc_stmts = block
                .iter()
                .map(|e| typecheck_expr(env, base_data, e))
                .collect::<CXResult<Vec<_>>>()?;

            TCExpr {
                _type: CXType::from(CXTypeKind::Unit),
                kind: TCExprKind::CSwitch {
                    condition: Box::new(tc_condition),
                    block: tc_stmts,
                    cases: cases.clone(),
                    default_case: *default_case,
                },
            }
        }

        CXExprKind::Match {
            condition,
            arms,
            default,
        } => {
            let mut match_value = typecheck_expr(env, base_data, condition)?;
            coerce_value(&mut match_value)?;

            match &match_value._type.kind {
                CXTypeKind::TaggedUnion {
                    name: expected_union_name,
                    variants,
                    ..
                } => {
                    let mut tc_arms = Vec::new();

                    for (value, block) in arms.iter() {
                        env.push_scope();

                        let CXExprKind::TypeConstructor {
                            union_name,
                            variant_name,
                            inner,
                        } = &value.kind
                        else {
                            log_typecheck_error!(
                                env,
                                value,
                                " Expected Type Constructor in 'match' arm, found: {}",
                                value
                            );
                        };

                        let CXExprKind::Identifier(instance_name) = &inner.as_ref().kind else {
                            log_typecheck_error!(
                                env,
                                inner.as_ref(),
                                " Expected identifier in 'match' arm, found: {}",
                                inner
                            );
                        };
                        let instance_name = instance_name.clone();

                        if union_name.as_str() != expected_union_name.as_str() {
                            log_typecheck_error!(
                                env,
                                value,
                                " Mismatched type in 'match' arm, expected '{}', found '{}'",
                                match_value._type,
                                union_name
                            );
                        }

                        let Some((idx, variant_type)) = variants
                            .iter()
                            .enumerate()
                            .find(|(_i, (name, _))| name == variant_name.as_str())
                            .map(|(i, (_, variant_type))| (i, variant_type.clone()))
                        else {
                            log_typecheck_error!(
                                env,
                                value,
                                " Variant '{}' not found in tagged union type {}",
                                variant_name,
                                union_name
                            );
                        };

                        env.insert_symbol(
                            instance_name.as_string(),
                            variant_type.clone().mem_ref_to(),
                        );
                        let tc_block = typecheck_expr(env, base_data, block)?;

                        tc_arms.push(TCTagMatch {
                            tag_value: idx as u64,
                            body: Box::new(tc_block),

                            instance_name,
                            variant_type,
                        });

                        env.pop_scope();
                    }

                    let default_case = default
                        .as_ref()
                        .map(|d| {
                            env.push_scope();
                            let Ok(tc_default) = typecheck_expr(env, base_data, d) else {
                                log_typecheck_error!(
                                    env,
                                    d,
                                    " Failed to typecheck default case in 'match' expression"
                                );
                            };
                            env.pop_scope();

                            Ok(Box::new(tc_default))
                        })
                        .transpose()?;

                    TCExpr {
                        _type: CXType::from(CXTypeKind::Unit),
                        kind: TCExprKind::Match {
                            condition: Box::new(match_value),
                            cases: tc_arms,
                            default_case,
                        },
                    }
                }

                _ => todo!("Integer-based matching"),
            }
        }

        CXExprKind::Taken => {
            unreachable!("Taken expressions should not be present in the typechecker")
        }
    })
}

pub(crate) fn global_expr(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    ident: &str,
) -> CXResult<TCExpr> {
    if let Some(global) = env.realized_globals.get(ident) {
        return tcglobal_expr(global);
    }

    let Some(module_res) = base_data
        .global_variables
        .get(ident) else {
            return CXError::create_result(format!("Global variable '{}' not found", ident));
        };

    let module_res = match env.in_external_templated_function {
        true => module_res.clone().transfer(""),
        false => module_res.clone(),
    };

    match &module_res.resource {
        CXGlobalVariable::EnumConstant(val) => Ok(TCExpr {
            _type: CXType::from(CXTypeKind::Integer {
                signed: true,
                bytes: 8,
            }),
            kind: TCExprKind::IntLiteral { value: *val as i64 },
        }),

        CXGlobalVariable::Standard {
            type_,
            initializer,
            is_mutable,
        } => {
            let _type = env.complete_type(base_data, type_)?;
            let initializer = match initializer.as_ref() {
                Some(init_expr) => {
                    let CXExprKind::IntLiteral { val, .. } = &init_expr.kind else {
                        log_typecheck_error!(
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
                TCGlobalVariable {
                    kind: TCGlobalVarKind::Variable {
                        name: CXIdent::from(ident.to_string()),
                        _type,
                        initializer,
                    },
                    is_mutable: *is_mutable,
                    linkage: module_res.linkage,
                },
            );

            tcglobal_expr(env.realized_globals.get(ident).unwrap())
        }
    }
}

fn tcglobal_expr(global: &TCGlobalVariable) -> CXResult<TCExpr> {
    match &global.kind {
        TCGlobalVarKind::Variable { name, _type, .. } => Ok(TCExpr {
            _type: _type.clone().mem_ref_to(),
            kind: TCExprKind::GlobalVariableReference { name: name.clone() },
        }),

        TCGlobalVarKind::StringLiteral { .. } => {
            unreachable!("String literals cannot be referenced via an identifier")
        }
    }
}
