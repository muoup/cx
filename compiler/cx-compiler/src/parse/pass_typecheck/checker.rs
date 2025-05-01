use std::env::args;
use crate::log_error;
use crate::mangling::member_function_mangle;
use crate::parse::pass_bytecode::typing::{get_intrinsic_type, struct_field_offset};
use crate::parse::value_type::CXValType;
use crate::parse::pass_molded::{CXBinOp, CXExpr, CXInitIndex};
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

        CXExpr::Assignment { lhs, rhs, .. } => {
            let lhs_type = type_check_traverse(env, lhs)?;
            implicit_coerce(env, rhs.as_mut(), lhs_type.clone())?;

            Some(lhs_type)
        },
        CXExpr::BinOp { lhs, rhs, op: CXBinOp::Access } => {
            let lhs_type = type_check_traverse(env, lhs.as_mut())?;

            match rhs.as_mut() {
                CXExpr::VarReference(name) => {
                    let Some(record) = struct_access(env, &lhs_type, name) else {
                        log_error!("TYPE ERROR: Unknown field {name} of structured type {lhs_type}");
                    };

                    let CXExpr::BinOp { lhs, .. } = std::mem::replace(expr, CXExpr::VarReference("".to_string())) else {
                        unreachable!()
                    };

                    *expr = CXExpr::StructAccess {
                        expr: lhs,
                        field: record.field_name,
                        field_type: record.field_type.clone(),

                        field_offset: record.field_offset,
                        field_index: record.field_index,
                    };

                    Some(record.field_type)
                },

                CXExpr::DirectFunctionCall { name, args } => {
                    let CXValType::Identifier(struct_name) = lhs_type else {
                        log_error!("TYPE ERROR: Cannot access function {name} on non-structured type {lhs_type}");
                    };

                    let mangled_name = member_function_mangle(
                        struct_name.as_str(),
                        name.as_str()
                    );

                    *expr = CXExpr::DirectFunctionCall {
                        name: mangled_name,
                        args: std::mem::take(args)
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

        CXExpr::VarDeclaration { name, type_, initializer } => {
            if let Some(initializer) = initializer {
                implicit_coerce(env, initializer, type_.clone())?;
                env.symbol_table.insert(name.clone(), type_.clone());
                return Some(type_.clone());
            }

            Some(CXValType::Unit)
        },

        CXExpr::VarReference(name) => {
            env.symbol_table.get(name).cloned()
        },

        CXExpr::IntLiteral { bytes, .. } => {
            Some(CXValType::Integer { bytes: *bytes, signed: true })
        },
        CXExpr::FloatLiteral { bytes, .. } => {
            Some(CXValType::Float { bytes: *bytes })
        },
        CXExpr::StringLiteral { .. } => {
            Some(CXValType::PointerTo(Box::new(CXValType::Identifier("char".to_string()))))
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
            let function_type = env.fn_map.get(name)?.clone();

            if function_type.parameters.len() != args.len() {
                log_error!("TYPE ERROR: Function {name} expected {} arguments, got {}", function_type.parameters.len(), args.len());
            }

            for (arg, proto_param) in args.iter_mut().zip(function_type.parameters.iter()) {
                implicit_coerce(env, arg, proto_param.type_.clone())?;
            }

            Some(function_type.return_type.clone())
        },

        CXExpr::InitializerList { indices } =>
            log_error!("Bare initializer list {indices:?} found in expression context"),

        // Generated only internally so types must be valid
        CXExpr::StructAccess { field_type, .. } => {
            Some(field_type.clone())
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
        format!("__internal_struct_{}", STRUCT_COUNTER - 1)
    };

    exprs.push(
        CXExpr::VarDeclaration {
            name: internal_struct_name.clone(),
            type_: CXValType::Structured {
                fields: type_indices.clone().to_vec()
            },
            initializer: None
        }
    );

    env.symbol_table.insert(
        internal_struct_name.clone(),
        to_type.clone()
    );

    for (i, index) in indices.into_iter().enumerate() {
        let (field_name, assn_to) = match index {
            CXInitIndex::Unnamed(assn_to) => {
                (type_indices.get(i).cloned()?.0, assn_to)
            },
            CXInitIndex::Named(name, assn_to) => {
                (name.clone(), assn_to)
            }
        };

        let Some(record) = struct_access(
            env,
            &CXValType::Structured { fields: type_indices.clone().to_vec() },
            &field_name
        ) else {
            log_error!("TYPE ERROR: Unknown field {field_name} of structured type {internal_struct_name}");
        };

        println!(".{} = {}", field_name, assn_to);

        let mut assign = CXExpr::Assignment {
            lhs: Box::new(CXExpr::StructAccess {
                expr: Box::new(CXExpr::VarReference(internal_struct_name.clone())),
                field: record.field_name,
                field_type: record.field_type.clone(),
                field_offset: record.field_offset,
                field_index: record.field_index,
            }),
            rhs: Box::new(*assn_to),
            op: None
        };

        type_check_traverse(env, &mut assign)?;

        exprs.push(assign)
    }

    Some(
        CXExpr::Block {
            exprs,
            value: Some(
                Box::new(
                    CXExpr::VarReference(internal_struct_name.clone())
                )
            )
        },
    )
}
