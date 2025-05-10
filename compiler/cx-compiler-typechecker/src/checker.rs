use std::any::Any;
use std::env::args;
use cx_compiler_ast::parse::operators::comma_separated_mut;
use cx_data_ast::parse::value_type::{get_intrinsic_type, is_structure, struct_field_access, CXTypeUnion, CXValType, CX_CONST};
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXInitIndex, CXUnOp};
use cx_data_ast::parse::identifier::CXIdent;
use cx_util::log_error;
use crate::struct_typechecking::access_struct;
use crate::{type_matches, TypeEnvironment};

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
                CXUnOp::AddressOf => {
                    let CXExpr::Identifier(_) = operand.as_ref() else {
                        log_error!("TYPE ERROR: AddressOf operator can only be applied to variables");
                    };

                    let mut operand_type = type_check_traverse(env, operand.as_mut())?;

                    Some(
                        CXValType::new(
                            0,
                            CXTypeUnion::PointerTo(Box::new(operand_type.clone()))
                        )
                    )
                },
                CXUnOp::Dereference => {
                    let operand_type = type_check_traverse(env, operand.as_mut())?;
                    let CXTypeUnion::PointerTo(deref) = get_intrinsic_type(env.type_map, &operand_type).cloned()? else {
                        log_error!("TYPE ERROR: Dereference operator can only be applied to pointers");
                    };

                    Some(
                        CXValType::new(
                            0,
                            CXTypeUnion::MemoryReference(deref)
                        )
                    )
                },
                CXUnOp::PostIncrement(_) |
                CXUnOp::PreIncrement(_) => {
                    let operand_type = type_check_traverse(env, operand.as_mut())?;

                    let CXTypeUnion::MemoryReference(inner) = operand_type.internal_type else {
                        log_error!("TYPE ERROR: Increment operator can only be applied to memory references, found: {operand}");
                    };

                    let CXTypeUnion::Integer { .. } = get_intrinsic_type(env.type_map, &inner).cloned()? else {
                        log_error!("TYPE ERROR: Increment operator can only be applied to integers, found: {inner}");
                    };

                    Some(*inner)
                },

                _ => todo!("type_check_traverse: {expr}")
            }
        },

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::Assign(_) } => {
            let lhs_type = type_check_traverse(env, lhs)?;
            let CXTypeUnion::MemoryReference(lhs_type) = lhs_type.internal_type else {
                log_error!("TYPE ERROR: Assignment operator can only be applied to memory references, found: {lhs_type}");
            };

            implicit_coerce(env, rhs, lhs_type.as_ref().clone())?;

            Some(*lhs_type)
        },

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::Access } =>
            access_struct(env, lhs.as_mut(), rhs.as_mut()),

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::MethodCall } => {
            let lhs_type = coerce_value(env, lhs)?;
            let CXTypeUnion::Function { return_type, args } = lhs_type.internal_type else {
                log_error!("TYPE ERROR: Method call operator can only be applied to functions, found: {lhs} of type {lhs_type}");
            };

            for (arg, expected_type) in
                comma_separated_mut(rhs)
                .into_iter()
                .zip(args.iter())
            {
                implicit_coerce(env, arg, expected_type.clone())?;
            }


            Some(*return_type)
        }

        CXExpr::BinOp { lhs, rhs, .. } => {
            let lhs_type = coerce_value(env, lhs)?;
            implicit_coerce(env, rhs, lhs_type.clone())?;

            Some(lhs_type)
        },

        CXExpr::VarDeclaration { name, type_ } => {
            env.symbol_table.insert(name.to_owned(), type_.clone());

            Some(
                CXValType::new(
                    0,
                    CXTypeUnion::MemoryReference(Box::new(type_.clone()))
                )
            )
        },

        CXExpr::Identifier(name) => {
            if let Some(record) = env.symbol_table.get(name.as_str()) {
                return Some(
                    CXValType::new(
                        0,
                        CXTypeUnion::MemoryReference(Box::new(record.clone()))
                    )
                );
            };

            if let Some(func) = env.fn_map.get(name.as_str()) {
                let return_type = func.return_type.clone();
                let args = func.parameters.iter()
                    .cloned()
                    .map(|param| param.type_)
                    .collect::<Vec<_>>();

                return Some(
                    CXValType::new(
                        0,
                        CXTypeUnion::Function {
                            return_type: Box::new(return_type),
                            args: args.into(),
                        }
                    )
                );
            };

            log_error!("TYPE ERROR: Unknown identifier {name}");
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
            let condition_type = type_check_traverse(env, condition)?;
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
            } else if env.return_type.is_intrinsic(&CXTypeUnion::Unit, &env.type_map) {
                log_error!("TYPE ERROR: Function with empty return in non-void context");
            }

            Some(CXValType::unit())
        },

        CXExpr::InitializerList { indices } =>
            log_error!("Bare initializer list {indices:?} found in expression context"),

        CXExpr::ImplicitCast { to_type, .. } => Some(to_type.clone()),

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

        _ => todo!("type_check_traverse: {expr}")
    }
}

pub fn implicit_coerce(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
    to_type: CXValType
) -> Option<()> {
    if matches!(expr, CXExpr::InitializerList { .. }) {
        todo!()
    }

    let from_type = coerce_value(env, expr)?;

    unsafe {
        // this feels like something that should be safe with the borrow checker, not my problem
        let old_expr = std::ptr::read(expr);
        let implicit_cast = CXExpr::ImplicitCast {
            expr: Box::new(old_expr),
            from_type,
            to_type,
        };

        let replaced = std::mem::replace(expr, implicit_cast);
        std::mem::forget(replaced);
    }

    Some(())
}

fn coerce_value(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
) -> Option<CXValType> {
    let expr_type = type_check_traverse(env, expr)?;

    let CXTypeUnion::MemoryReference(inner) = expr_type.internal_type else {
        return Some(expr_type);
    };

    let expr_temp = std::mem::replace(expr, CXExpr::Taken);
    *expr = CXExpr::ImplicitLoad {
        expr: Box::new(expr_temp),
        loaded_type: inner.as_ref().clone()
    };

    Some(*inner)
}
