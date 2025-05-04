use std::any::Any;
use std::env::args;
use crate::{log_error, type_matches};
use crate::parse::pass_bytecode::typing::{get_intrinsic_type, struct_field_offset};
use crate::parse::value_type::{is_structure, CXValType};
use crate::parse::pass_ast::{CXBinOp, CXExpr, CXInitIndex, CXUnOp};
use crate::parse::pass_ast::identifier::CXIdent;
use crate::parse::pass_ast::operators::{comma_separated, comma_separated_mut};
use crate::parse::pass_typecheck::struct_typechecking::access_struct;
use crate::parse::pass_typecheck::type_utils::{prototype_to_type, struct_field_access, type_matches};
use crate::parse::pass_typecheck::TypeEnvironment;

pub(crate) fn type_check_traverse(env: &mut TypeEnvironment, expr: &mut CXExpr) -> Option<CXValType> {
    match expr {
        CXExpr::Block { exprs, value } => {
            for expr in exprs {
                type_check_traverse(env, expr)?;
            }

            if let Some(value) = value {
                return type_check_traverse(env, value.as_mut());
            }

            Some(CXValType::Unit)
        },

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::Assign(_) } => {
            let lhs_type = type_check_traverse(env, lhs)?;
            implicit_coerce(env, rhs.as_mut(), lhs_type.clone())?;

            Some(lhs_type)
        },

        CXExpr::UnOp { operator, operand } => {
            match operator {
                CXUnOp::AddressOf => {
                    let CXExpr::Identifier(_) = operand.as_ref() else {
                        log_error!("TYPE ERROR: AddressOf operator can only be applied to variables");
                    };

                    let mut operand_type = type_check_traverse(env, operand.as_mut())?;

                    Some(CXValType::PointerTo(Box::new(operand_type.clone())))
                },
                CXUnOp::Dereference => {
                    let operand_type = type_check_traverse(env, operand.as_mut())?;
                    let CXValType::PointerTo(deref) = get_intrinsic_type(env.type_map, &operand_type).cloned()? else {
                        log_error!("TYPE ERROR: Dereference operator can only be applied to pointers");
                    };

                    Some(*deref)
                },

                _ => todo!("type_check_traverse: {expr}")
            }
        },

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::Access } =>
            access_struct(env, lhs.as_mut(), rhs.as_mut()),

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::MethodCall } => {
            let lhs_type = type_check_traverse(env, lhs)?;
            let CXValType::Function { return_type, args } = lhs_type else {
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
            let lhs_type = type_check_traverse(env, lhs)?;

            implicit_coerce(env, lhs, lhs_type.clone())?;
            implicit_coerce(env, rhs, lhs_type.clone())?;

            Some(lhs_type)
        },

        CXExpr::VarDeclaration { name, type_ } => {
            env.symbol_table.insert(name.to_owned(), type_.clone());

            Some(type_.clone())
        },

        CXExpr::Identifier(name) => {
            if let Some(record) = env.symbol_table.get(name.as_str()) {
                return Some(record.clone());
            };

            if let Some(func) = env.fn_map.get(name.as_str()) {
                let return_type = func.return_type.clone();
                let args = func.parameters.iter()
                    .cloned()
                    .map(|param| param.type_)
                    .collect::<Vec<_>>();

                return Some(
                    CXValType::Function {
                        return_type: Box::new(return_type),
                        args: args.into(),
                    }
                );
            };

            log_error!("TYPE ERROR: Unknown identifier {name}");
        },

        CXExpr::IntLiteral { bytes, .. } => {
            Some(CXValType::Integer { bytes: *bytes, signed: true })
        },
        CXExpr::FloatLiteral { bytes, .. } => {
            Some(CXValType::Float { bytes: *bytes })
        },
        CXExpr::StringLiteral { .. } => {
            Some(CXValType::PointerTo(Box::new(CXValType::Identifier(CXIdent::from("char")))))
        },

        CXExpr::If { condition, then_branch, else_branch } => {
            let condition_type = type_check_traverse(env, condition)?;
            if !type_matches!(env, &condition_type, &CXValType::Integer) {
                log_error!("TYPE ERROR: If condition must be of type bool, found: {condition_type}");
            }

            let then_type = type_check_traverse(env, then_branch)?;
            if let Some(else_branch) = else_branch {
                let else_type = type_check_traverse(env, else_branch)?;
                if !type_matches!(env, &then_type, &else_type) {
                    log_error!("TYPE ERROR: If branches must have the same type, found: {then_type} and {else_type}");
                }
            }

            Some(CXValType::Unit)
        },

        CXExpr::Return { value } => {
            if let Some(value) = value {
                implicit_coerce(env, value, env.return_type.clone())?;
            } else if !type_matches(env, &env.return_type, &CXValType::Unit)? {
                log_error!("TYPE ERROR: Function with empty return in non-void context");
            }

            Some(CXValType::Unit)
        },

        CXExpr::InitializerList { indices } =>
            log_error!("Bare initializer list {indices:?} found in expression context"),

        CXExpr::ImplicitCast { to_type, .. } => Some(to_type.clone()),

        CXExpr::For { init, condition, increment, body } => {
            type_check_traverse(env, init)?;
            let condition_type = type_check_traverse(env, condition)?;
            type_check_traverse(env, increment)?;
            type_check_traverse(env, body)?;

            if !type_matches!(env, &condition_type, &CXValType::Integer { .. }) {
                log_error!("TYPE ERROR: For loop condition must have condition type, found: {condition_type}");
            }

            Some(CXValType::Unit)
        },

        _ => todo!("type_check_traverse: {expr}")
    }
}

pub(crate) fn implicit_coerce(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
    to_type: CXValType
) -> Option<()> {
    if matches!(expr, CXExpr::InitializerList { .. }) {
        let CXExpr::InitializerList { indices } = std::mem::replace(expr, CXExpr::Taken) else {
            unreachable!()
        };

        *expr = coerce_struct_initializer(
            env,
            to_type,
            indices
        )?;

        return Some(());
    }

    let from_type = type_check_traverse(env, expr)?;

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

fn coerce_struct_initializer(
    env: &mut TypeEnvironment,
    to_type: CXValType,
    indices: Vec<CXInitIndex>,
) -> Option<CXExpr> {
    static mut STRUCT_COUNTER: usize = 0;
    let mut exprs = vec![];

    let CXValType::Structured { fields: type_indices } = get_intrinsic_type(env.type_map, &to_type).cloned()? else {
        log_error!("TYPE ERROR: Cannot coerce initializer list to non-structured type {to_type}");
    };

    // lol
    let internal_struct_name = unsafe {
        STRUCT_COUNTER += 1;
        CXIdent::from(format!("__internal_struct_{}", STRUCT_COUNTER - 1).as_str())
    };

    exprs.push(
        CXExpr::VarDeclaration {
            name: internal_struct_name.clone(),
            type_: CXValType::Structured {
                fields: type_indices.clone().to_vec()
            }
        }
    );

    env.symbol_table.insert(
        internal_struct_name.to_owned(),
        to_type.clone()
    );

    for (i, index) in indices.into_iter().enumerate() {
        let (field_name, assn_to) = match index {
            CXInitIndex::Unnamed(assn_to) => {
                (CXIdent::from(type_indices.get(i).cloned()?.0.as_str()), assn_to)
            },
            CXInitIndex::Named(name, assn_to) => {
                (name, assn_to)
            }
        };

        let Some(record) = struct_field_access(
            env.type_map,
            &CXValType::Structured { fields: type_indices.clone().to_vec() },
            field_name.as_str()
        ) else {
            log_error!("TYPE ERROR: Unknown field {field_name} of structured type {internal_struct_name}");
        };

        let access = CXExpr::BinOp {
            lhs: Box::new(
                CXExpr::Identifier(internal_struct_name.clone())
            ),
            rhs: Box::new(
                CXExpr::Identifier(field_name)
            ),
            op: CXBinOp::Access
        };

        let mut assignment = CXExpr::BinOp {
            lhs: Box::new(access),
            rhs: assn_to,
            op: CXBinOp::Assign(None)
        };

        type_check_traverse(env, &mut assignment)?;

        exprs.push(assignment)
    }

    Some(
        CXExpr::Block {
            exprs,
            value: Some(
                Box::new(
                    CXExpr::Identifier(CXIdent::from(internal_struct_name.as_str()))
                )
            )
        },
    )
}
