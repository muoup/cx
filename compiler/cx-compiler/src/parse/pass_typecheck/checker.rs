use std::any::Any;
use std::env::args;
use crate::log_error;
use crate::mangling::namespace_mangle;
use crate::parse::pass_bytecode::typing::{get_intrinsic_type, struct_field_offset};
use crate::parse::value_type::CXValType;
use crate::parse::pass_ast::{CXBinOp, CXExpr, CXInitIndex, CXUnOp};
use crate::parse::pass_ast::identifier::CXIdent;
use crate::parse::pass_typecheck::type_utils::{struct_access, type_matches};
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

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::Access } => {
            let mut lhs_type = type_check_traverse(env, lhs.as_mut())?;

            match rhs.as_mut() {
                CXExpr::Identifier(name) => {
                    // Auto dereference pointers
                    if matches!(lhs_type, CXValType::PointerTo(_)) {
                        let lhs_temp = std::mem::replace(lhs, Box::new(CXExpr::Taken));
                        *lhs = Box::new(
                            CXExpr::UnOp {
                                operand: lhs_temp,
                                operator: CXUnOp::Dereference
                            }
                        );

                        let CXValType::PointerTo(inner_type) = lhs_type else {
                            unreachable!()
                        };

                        lhs_type = *inner_type;
                    };

                    let Some(record) = struct_access(env.type_map, &lhs_type, name.as_str()) else {
                        let as_intrinsic = format!("{:?}", get_intrinsic_type(env.type_map, &lhs_type));
                        log_error!("TYPE ERROR: Unknown field {name} of structured type {lhs_type}, type: {as_intrinsic}");
                    };

                    Some(record.field_type)
                },

                CXExpr::DirectFunctionCall { name, args } => {
                    let CXValType::Identifier(struct_name) = lhs_type else {
                        log_error!("TYPE ERROR: Cannot access function {name} on non-structured type {lhs_type}");
                    };

                    let mangled_name = namespace_mangle(
                        &vec![&struct_name, name]
                    );

                    let lhs_taken = std::mem::replace(lhs, Box::new(CXExpr::Taken));
                    let mut member_args = vec![
                        CXExpr::UnOp { operator: CXUnOp::AddressOf, operand: lhs_taken }
                    ];
                    member_args.extend(std::mem::take(args));

                    *expr = CXExpr::DirectFunctionCall {
                        name: CXIdent::from(mangled_name.as_str()),
                        args: member_args
                    };

                    type_check_traverse(env, expr)
                },

                _ => log_error!("TYPE ERROR: Cannot access field {rhs:?} of structured type {lhs_type}")
            }
        },
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
            let Some(record) = env.symbol_table.get(name.as_str()) else {
                log_error!("TYPE ERROR: Unknown variable {name}\n Variables: {:?}", env.symbol_table);
            };

            Some(record.clone())
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

        CXExpr::Return { value } => {
            if let Some(value) = value {
                implicit_coerce(env, value, env.return_type.clone())?;
            } else if !type_matches(env, &env.return_type, &CXValType::Unit)? {
                log_error!("TYPE ERROR: Function with empty return in non-void context");
            }

            Some(CXValType::Unit)
        },

        CXExpr::DirectFunctionCall { name, args } => {
            let function_type = env.fn_map.get(name.as_str())?.clone();

            if function_type.parameters.len() != args.len() {
                log_error!("TYPE ERROR: Function {name} expected {} arguments, got {}", function_type.parameters.len(), args.len());
            }

            for (arg, proto_param) in
                args.iter_mut().zip(function_type.parameters.iter()) {
                implicit_coerce(env, arg, proto_param.type_.clone())?;
            }

            Some(function_type.return_type.clone())
        },

        CXExpr::InitializerList { indices } =>
            log_error!("Bare initializer list {indices:?} found in expression context"),

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

        let Some(record) = struct_access(
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
