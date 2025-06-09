use cx_compiler_ast::parse::operators::{comma_separated_mut};
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXExprKind, CXUnOp};
use cx_data_ast::parse::value_type::{get_intrinsic_type, same_type, CXTypeKind, CXType, CX_CONST};
use cx_util::log_error;
use crate::struct_typechecking::access_struct;
use crate::casting::{alg_bin_op_coercion, explicit_cast, implicit_cast};
use crate::TypeEnvironment;

pub(crate) fn type_check_traverse<'a>(env: &'a mut TypeEnvironment, expr: &mut CXExpr) -> Option<&'a CXType> {
    let _type = type_check_inner(env, expr)?;

    env.expr_type_map.insert(expr, _type.clone())
}

fn type_check_inner(env: &mut TypeEnvironment, expr: &mut CXExpr) -> Option<CXType> {
    match &mut expr.kind {
        CXExprKind::Block { exprs, value } => {
            for expr in exprs {
                type_check_traverse(env, expr)?;
            }

            if let Some(value) = value {
                return type_check_traverse(env, value.as_mut()).cloned();
            }

            Some(CXType::unit())
        },

        CXExprKind::UnOp { operator, operand } => {
            match operator {
                CXUnOp::Negative => { 
                    coerce_value(env, operand.as_mut())
                },
                
                CXUnOp::AddressOf => {
                    let operand_type = type_check_traverse(env, operand.as_mut())?.clone();

                    match operand_type.intrinsic_type(&env.type_map)? {
                        CXTypeKind::MemoryAlias(inner) => Some(inner.clone().pointer_to()),
                        CXTypeKind::Function { .. } => coerce_value(env, operand.as_mut()),

                        _ => log_error!("TYPE ERROR: Cannot take address of expr: {operand_type}"),
                    }
                },
                CXUnOp::Dereference => {
                    let operand_type = coerce_value(env, operand.as_mut())?;
                    let CXTypeKind::PointerTo(deref) = get_intrinsic_type(env.type_map, &operand_type).cloned()? else {
                        log_error!("TYPE ERROR: Dereference operator can only be applied to pointers, found {operand} of type {operand_type}");
                    };

                    Some(
                        CXType::new(
                            0,
                            CXTypeKind::MemoryAlias(deref)
                        )
                    )
                },
                CXUnOp::PostIncrement(_) |
                CXUnOp::PreIncrement(_) => {
                    let operand_type = type_check_traverse(env, operand.as_mut())?.clone();

                    let CXTypeKind::MemoryAlias(inner) = &operand_type.kind else {
                        log_error!("TYPE ERROR: Increment operator can only be applied to memory references, found: {operand}");
                    };

                    let CXTypeKind::Integer { .. } = get_intrinsic_type(env.type_map, &inner).cloned()? else {
                        log_error!("TYPE ERROR: Increment operator can only be applied to integers, found: {inner}");
                    };

                    Some(*inner.clone())
                },

                CXUnOp::ExplicitCast (to_type) => {
                    let mut operand_type = coerce_value(env, operand.as_mut())?;

                    let Some(_) = explicit_cast(env, operand, &mut operand_type, to_type) else {
                        log_error!("TYPE ERROR: Invalid cast from {operand_type} to {to_type}");
                    };

                    Some(to_type.clone())
                },

                _ => todo!("CXUnOp {operator:?} not implemented"),
            }
        },

        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::Assign(_) } => {
            let lhs_type = type_check_traverse(env, lhs)?.clone();
            let CXTypeKind::MemoryAlias(lhs_type) = &lhs_type.kind else {
                log_error!("TYPE ERROR: Assignment operator can only be applied to memory references, found: {lhs_type}");
            };

            if lhs_type.as_ref().get_specifier(CX_CONST) {
                log_error!("TYPE ERROR: Assignment operator cannot be applied to const variables");
            }

            implicit_coerce(env, rhs, lhs_type.as_ref().clone())?;

            Some(*lhs_type.clone())
        },

        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::Access } =>
            access_struct(env, lhs.as_mut(), rhs.as_mut()),

        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::ArrayIndex } => {
            let end_type = alg_bin_op_coercion(env, lhs, rhs)?;

            let CXTypeKind::PointerTo(inner) = end_type.intrinsic_type(env.type_map)? else {
                log_error!("TYPE ERROR: Array index operator can only be applied to pointers, found: {end_type}");
            };
            
            Some(
                CXType::new(
                    end_type.specifiers,
                    CXTypeKind::MemoryAlias(inner.clone())
                )
            )
        },

        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::MethodCall } => {
            let mut lhs_type = coerce_mem_ref(env, lhs)?;

            if let Some(CXTypeKind::PointerTo(inner)) = lhs_type.intrinsic_type(&env.type_map) {
                lhs_type = *inner.clone();
            }

            let CXTypeKind::Function { prototype } = lhs_type.intrinsic_type(&env.type_map).cloned()? else {
                log_error!("TYPE ERROR: Method call operator can only be applied to functions, found: {lhs} of type {lhs_type}");
            };

            let mut args = comma_separated_mut(rhs);

            if args.len() != prototype.params.len() && !prototype.var_args {
                log_error!("TYPE ERROR: Method call operator expects {} arguments, found {}", prototype.params.len(), args.len());
            }

            for (arg, expected_type) in
                args
                .iter_mut()
                .zip(prototype.params.iter())
            {
                implicit_coerce(env, arg, expected_type.type_.clone())?;
            }

            for i in prototype.params.len()..args.len() {
                let va_type = coerce_value(env, args[i])?;

                if va_type.is_structure(&env.type_map) {
                    log_error!("TYPE ERROR: Cannot pass structure type as vararg");
                }
            }

            Some(prototype.return_type.clone())
        }

        CXExprKind::BinOp { lhs, rhs, .. } =>
            alg_bin_op_coercion(env, lhs, rhs),

        CXExprKind::VarDeclaration { name, type_ } => {
            env.symbol_table.insert(name.as_string(), type_.clone());

            let modified_type = type_.clone().remove_specifier(CX_CONST);

            Some(
                CXType::new(
                    0,
                    CXTypeKind::MemoryAlias(Box::new(modified_type))
                )
            )
        },

        CXExprKind::Identifier(name) => {
            if let Some(record) = env.symbol_table.get(name.as_str()).cloned() {
                return match record.intrinsic_type(env.type_map)? {
                    // Array variables are themselves memory aliases, so wrapping
                    // them in a memory alias ends up adding an extra load operation
                    // when using them
                    CXTypeKind::Array { .. } |
                    CXTypeKind::Structured { .. } => {
                        Some(record)
                    },
                    
                    _ => {
                        Some(
                            CXType::new(
                                record.specifiers,
                                CXTypeKind::MemoryAlias(Box::new(record))
                            )
                        )
                    },
                };
            };

            if let Some(func) = env.fn_map.get(name.as_str()) {
                return Some(
                    CXTypeKind::Function { prototype: Box::new(func.clone()) }.to_val_type()
                );
            };

            log_error!("TYPE ERROR: Unknown identifier {name}");
        },

        CXExprKind::IntLiteral { bytes, .. } => {
            Some(
                CXType::new(
                    0,
                    CXTypeKind::Integer { bytes: *bytes, signed: true }
                )
            )
        },
        CXExprKind::FloatLiteral { bytes, .. } => {
            Some(
                CXType::new(
                    0,
                    CXTypeKind::Float { bytes: *bytes }
                )
            )
        },
        CXExprKind::StringLiteral { .. } => {
            Some(
                CXType::new(
                    0,
                    CXTypeKind::PointerTo(
                        Box::new(
                            CXType::new(
                                CX_CONST,
                                CXTypeKind::Integer { bytes: 1, signed: true }
                            )
                        )
                    )
                )
            )
        },

        CXExprKind::If { condition, then_branch, else_branch } => {
            let condition_type = coerce_value(env, condition)?;
            if !matches!(condition_type.intrinsic_type(&env.type_map), Some(CXTypeKind::Integer { .. })) {
                log_error!("TYPE ERROR: If condition must be of type int, found: {condition_type}");
            }

            type_check_traverse(env, then_branch)?;
            if let Some(else_branch) = else_branch {
                type_check_traverse(env, else_branch)?;
            }

            Some(CXType::unit())
        },

        CXExprKind::Return { value } => {
            if let Some(value) = value {
                implicit_coerce(env, value, env.return_type.clone())?;
            } else if !env.return_type.is_void(&env.type_map) {
                log_error!("TYPE ERROR: Function with empty return in non-void context");
            }

            Some(CXType::unit())
        },

        CXExprKind::InitializerList { indices } =>
            log_error!("Bare initializer list {indices:?} found in expression context"),

        CXExprKind::ImplicitCast { to_type, .. } => Some(to_type.clone()),
        CXExprKind::ImplicitLoad { loaded_type, .. } => Some(loaded_type.clone()),
        CXExprKind::GetFunctionAddr { func_sig, .. } => Some(func_sig.clone().pointer_to()),

        CXExprKind::For { init, condition, increment, body } => {
            type_check_traverse(env, init)?;
            let condition_type = type_check_traverse(env, condition)?.clone();
            type_check_traverse(env, increment)?;
            type_check_traverse(env, body)?;

            if matches!(get_intrinsic_type(env.type_map, &condition_type), Some(CXTypeKind::Integer { bytes: 1, signed: true })) {
                log_error!("TYPE ERROR: For loop condition must have condition type, found: {condition_type}");
            }

            Some(CXType::unit())
        },

        CXExprKind::While { condition, body, .. } => {
            let condition_type = type_check_traverse(env, condition)?.clone();

            if !matches!(condition_type.intrinsic_type(env.type_map)?, CXTypeKind::Integer { .. }) {
                implicit_coerce(env, condition, CXTypeKind::Integer { signed: true, bytes: 8 }.to_val_type())?;
            }

            type_check_traverse(env, body)?;

            Some(CXType::unit())
        },

        CXExprKind::Unit |
        CXExprKind::Break |
        CXExprKind::Continue => Some(CXType::unit()),

        _ => todo!("type_check_traverse: {expr}")
    }
}

pub(crate) fn implicit_coerce(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
    to_type: CXType
) -> Option<()> {
    let from_type = coerce_value(env, expr)?;

    if same_type(env.type_map, &from_type, &to_type) {
        return Some(());
    }

    let Some(()) = implicit_cast(env, expr, &from_type, &to_type)? else {
        log_error!("TYPE ERROR: Cannot implicitly cast {from_type} to {to_type}");
    };

    Some(())
}

fn coerce_mem_ref(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
) -> Option<CXType> {
    let expr_type = type_check_traverse(env, expr)?.clone();

    let CXTypeKind::MemoryAlias(inner)
        = expr_type.intrinsic_type(env.type_map).cloned()? else {
        return Some(expr_type);
    };

    let expr_temp = std::mem::take(expr);
    *expr = CXExprKind::ImplicitLoad {
        expr: Box::new(expr_temp),
        loaded_type: inner.as_ref().clone()
    }.into();
    type_check_traverse(env, expr).cloned()
}

pub(crate) fn coerce_value(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
) -> Option<CXType> {
    let expr_type = coerce_mem_ref(env, expr)?;

    match expr_type.intrinsic_type(env.type_map)? {
        CXTypeKind::Array { _type, .. } => Some(
            CXType::new(
                expr_type.specifiers,
                CXTypeKind::PointerTo(_type.clone())
            )
        ),
        CXTypeKind::Function { .. } => {
            let expr_temp = std::mem::take(expr);
            *expr = CXExprKind::GetFunctionAddr {
                func_name: Box::new(expr_temp),
                func_sig: expr_type.clone()
            }.into();
            type_check_traverse(env, expr).cloned()
        },
        _ => Some(expr_type)
    }
}
