use std::clone;
use std::env::args;
use cx_compiler_ast::parse::operators::{comma_separated, comma_separated_mut};
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXUnOp};
use cx_data_ast::parse::value_type::{get_intrinsic_type, is_structure, same_type, struct_field_access, CXTypeUnion, CXValType, CX_CONST};
use cx_util::log_error;
use crate::struct_typechecking::access_struct;
use crate::casting::{alg_bin_op_coercion, explicit_cast, implicit_cast, valid_implicit_cast};
use crate::TypeEnvironment;

pub fn type_check_traverse(env: &mut TypeEnvironment, expr: &mut CXExpr) -> Option<CXValType> {
    match expr {
        CXExpr::Block { exprs, value } => {
            for expr in exprs {
                type_check_traverse(env, expr)?;
            }

            if let Some(value) = value {
                return type_check_traverse(env, value.as_mut());
            }

            Some(CXValType::unit())
        },

        CXExpr::UnOp { operator, operand } => {
            match operator {
                CXUnOp::Negative => { 
                    coerce_value(env, operand.as_mut())
                },
                
                CXUnOp::AddressOf => {
                    let operand_type = type_check_traverse(env, operand.as_mut())?;

                    match operand_type.intrinsic_type(&env.type_map)? {
                        CXTypeUnion::MemoryAlias(inner) => Some(inner.clone().pointer_to()),
                        CXTypeUnion::Function { .. } => coerce_value(env, operand.as_mut()),

                        _ => log_error!("TYPE ERROR: Cannot take address of expr: {operand_type}"),
                    }
                },
                CXUnOp::Dereference => {
                    let operand_type = coerce_value(env, operand.as_mut())?;
                    let CXTypeUnion::PointerTo(deref) = get_intrinsic_type(env.type_map, &operand_type).cloned()? else {
                        log_error!("TYPE ERROR: Dereference operator can only be applied to pointers, found {operand} of type {operand_type}");
                    };

                    Some(
                        CXValType::new(
                            0,
                            CXTypeUnion::MemoryAlias(deref)
                        )
                    )
                },
                CXUnOp::PostIncrement(_) |
                CXUnOp::PreIncrement(_) => {
                    let operand_type = type_check_traverse(env, operand.as_mut())?;

                    let CXTypeUnion::MemoryAlias(inner) = operand_type.internal_type else {
                        log_error!("TYPE ERROR: Increment operator can only be applied to memory references, found: {operand}");
                    };

                    let CXTypeUnion::Integer { .. } = get_intrinsic_type(env.type_map, &inner).cloned()? else {
                        log_error!("TYPE ERROR: Increment operator can only be applied to integers, found: {inner}");
                    };

                    Some(*inner)
                },

                CXUnOp::ExplicitCast (to_type) => {
                    let mut operand_type = coerce_value(env, operand.as_mut())?;

                    let Some(_) = explicit_cast(env, operand.as_mut(), &mut operand_type, to_type) else {
                        log_error!("TYPE ERROR: Invalid cast from {operand_type} to {to_type}");
                    };

                    Some(to_type.clone())
                },

                _ => todo!("CXUnOp {operator:?} not implemented"),
            }
        },

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::Assign(_) } => {
            let lhs_type = type_check_traverse(env, lhs)?;
            let CXTypeUnion::MemoryAlias(lhs_type) = lhs_type.internal_type else {
                log_error!("TYPE ERROR: Assignment operator can only be applied to memory references, found: {lhs_type}");
            };

            if lhs_type.as_ref().get_specifier(CX_CONST) {
                log_error!("TYPE ERROR: Assignment operator cannot be applied to const variables");
            }

            implicit_coerce(env, rhs, lhs_type.as_ref().clone())?;

            Some(*lhs_type)
        },

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::Access } =>
            access_struct(env, lhs.as_mut(), rhs.as_mut()),

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::ArrayIndex } => {
            let end_type = alg_bin_op_coercion(env, lhs, rhs.as_mut())?;

            let CXTypeUnion::PointerTo(inner) = end_type.intrinsic_type(env.type_map)? else {
                log_error!("TYPE ERROR: Array index operator can only be applied to pointers, found: {end_type}");
            };
            
            Some(
                CXValType::new(
                    end_type.specifiers,
                    CXTypeUnion::MemoryAlias(inner.clone())
                )
            )
        },

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::MethodCall } => {
            let mut lhs_type = coerce_mem_ref(env, lhs)?;

            if let Some(CXTypeUnion::PointerTo(inner)) = lhs_type.intrinsic_type(&env.type_map) {
                lhs_type = *inner.clone();
            }

            let CXTypeUnion::Function { prototype } = lhs_type.intrinsic_type(&env.type_map).cloned()? else {
                log_error!("TYPE ERROR: Method call operator can only be applied to functions, found: {lhs} of type {lhs_type}");
            };

            let mut args = comma_separated_mut(rhs);

            if args.len() != prototype.parameters.len() && !prototype.var_args {
                log_error!("TYPE ERROR: Method call operator expects {} arguments, found {}", prototype.parameters.len(), args.len());
            }

            for (arg, expected_type) in
                args
                .iter_mut()
                .zip(prototype.parameters.iter())
            {
                implicit_coerce(env, arg, expected_type.type_.clone())?;
            }

            for i in prototype.parameters.len()..args.len() {
                let va_type = coerce_value(env, args[i])?;

                if va_type.is_structure(&env.type_map) {
                    log_error!("TYPE ERROR: Cannot pass structure type as vararg");
                }
            }

            Some(prototype.return_type.clone())
        }

        CXExpr::BinOp { lhs, rhs, .. } =>
            alg_bin_op_coercion(env, lhs.as_mut(), rhs.as_mut()),

        CXExpr::VarDeclaration { name, type_ } => {
            env.symbol_table.insert(name.to_owned(), type_.clone());

            let modified_type = type_.clone().remove_specifier(CX_CONST);

            Some(
                CXValType::new(
                    0,
                    CXTypeUnion::MemoryAlias(Box::new(modified_type))
                )
            )
        },

        CXExpr::Identifier(name) => {
            if let Some(record) = env.symbol_table.get(name.as_str()).cloned() {
                return match record.intrinsic_type(env.type_map)? {
                    // Array variables are themselves memory aliases, so wrapping
                    // them in a memory alias ends up adding an extra load operation
                    // when using them
                    CXTypeUnion::Array { .. } |
                    CXTypeUnion::Structured { .. } => {
                        Some(record)
                    },
                    
                    _ => {
                        Some(
                            CXValType::new(
                                record.specifiers,
                                CXTypeUnion::MemoryAlias(Box::new(record))
                            )
                        )
                    },
                };
            };

            if let Some(func) = env.fn_map.get(name.as_str()) {
                return Some(
                    CXTypeUnion::Function { prototype: Box::new(func.clone()) }.to_val_type()
                );
            };

            log_error!("TYPE ERROR: Unknown identifier {name}, fn_map dump: \n{:#?}", env.fn_map);
        },

        CXExpr::IntLiteral { bytes, .. } => {
            Some(
                CXValType::new(
                    0,
                    CXTypeUnion::Integer { bytes: *bytes, signed: true }
                )
            )
        },
        CXExpr::FloatLiteral { bytes, .. } => {
            Some(
                CXValType::new(
                    0,
                    CXTypeUnion::Float { bytes: *bytes }
                )
            )
        },
        CXExpr::StringLiteral { .. } => {
            Some(
                CXValType::new(
                    0,
                    CXTypeUnion::PointerTo(
                        Box::new(
                            CXValType::new(
                                CX_CONST,
                                CXTypeUnion::Integer { bytes: 1, signed: true }
                            )
                        )
                    )
                )
            )
        },

        CXExpr::If { condition, then_branch, else_branch } => {
            let condition_type = coerce_value(env, condition)?;
            if !matches!(condition_type.intrinsic_type(&env.type_map), Some(CXTypeUnion::Integer { .. })) {
                log_error!("TYPE ERROR: If condition must be of type int, found: {condition_type}");
            }

            type_check_traverse(env, then_branch)?;
            if let Some(else_branch) = else_branch {
                type_check_traverse(env, else_branch)?;
            }

            Some(CXValType::unit())
        },

        CXExpr::Return { value } => {
            if let Some(value) = value {
                implicit_coerce(env, value, env.return_type.clone())?;
            } else if !env.return_type.is_void(&env.type_map) {
                log_error!("TYPE ERROR: Function with empty return in non-void context");
            }

            Some(CXValType::unit())
        },

        CXExpr::InitializerList { indices } =>
            log_error!("Bare initializer list {indices:?} found in expression context"),

        CXExpr::ImplicitCast { to_type, .. } => Some(to_type.clone()),
        CXExpr::ImplicitLoad { loaded_type, .. } => Some(loaded_type.clone()),
        CXExpr::GetFunctionAddr { func_sig, .. } => Some(func_sig.clone().pointer_to()),

        CXExpr::For { init, condition, increment, body } => {
            type_check_traverse(env, init)?;
            let condition_type = type_check_traverse(env, condition)?;
            type_check_traverse(env, increment)?;
            type_check_traverse(env, body)?;

            if matches!(get_intrinsic_type(env.type_map, &condition_type), Some(CXTypeUnion::Integer { bytes: 1, signed: true })) {
                log_error!("TYPE ERROR: For loop condition must have condition type, found: {condition_type}");
            }

            Some(CXValType::unit())
        },

        CXExpr::While { condition, body, .. } => {
            let condition_type = type_check_traverse(env, condition)?;

            if !matches!(condition_type.intrinsic_type(env.type_map)?, CXTypeUnion::Integer { .. }) {
                implicit_coerce(env, condition, CXTypeUnion::Integer { signed: true, bytes: 8 }.to_val_type())?;
            }

            type_check_traverse(env, body)?;

            Some(CXValType::unit())
        },

        CXExpr::Unit |
        CXExpr::Break |
        CXExpr::Continue => Some(CXValType::unit()),

        _ => todo!("type_check_traverse: {expr}")
    }
}

pub fn implicit_coerce(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
    to_type: CXValType
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
) -> Option<CXValType> {
    let expr_type = type_check_traverse(env, expr)?;

    let CXTypeUnion::MemoryAlias(inner)
        = expr_type.intrinsic_type(env.type_map).cloned()? else {
        return Some(expr_type);
    };

    let expr_temp = std::mem::replace(expr, CXExpr::Taken);
    *expr = CXExpr::ImplicitLoad {
        expr: Box::new(expr_temp),
        loaded_type: inner.as_ref().clone()
    };

    type_check_traverse(env, expr)
}

pub(crate) fn coerce_value(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
) -> Option<CXValType> {
    let expr_type = coerce_mem_ref(env, expr)?;

    match expr_type.intrinsic_type(env.type_map)? {
        CXTypeUnion::Array { _type, .. } => Some(
            CXValType::new(
                expr_type.specifiers,
                CXTypeUnion::PointerTo(_type.clone())
            )
        ),
        CXTypeUnion::Function { .. } => {
            let expr_temp = std::mem::replace(expr, CXExpr::Taken);
            *expr = CXExpr::GetFunctionAddr {
                func_name: Box::new(expr_temp),
                func_sig: expr_type.clone()
            };
            type_check_traverse(env, expr)
        },
        _ => Some(expr_type)
    }
}
