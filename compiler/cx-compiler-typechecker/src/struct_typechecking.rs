use cx_compiler_ast::parse::operators::comma_separated_mut;
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXExprKind, CXFunctionPrototype, CXUnOp};
use cx_data_ast::parse::{CXFunctionIdentifier, CXObjectIdentifier};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::type_mapping::contextualize_template_args;
use cx_data_ast::parse::value_type::{CXTypeKind, CXType};
use cx_data_ast::preparse::pp_type::{CXNaiveType, CXNaiveTypeKind, PredeclarationType};
use cx_util::{log_error};
use crate::casting::implicit_cast;
use crate::checker::{coerce_mem_ref, coerce_value, implicit_coerce, type_check_traverse};
use crate::TypeEnvironment;

fn typecheck_member_function(
    env: &mut TypeEnvironment,
    lhs_type: &CXType,
    lhs: &mut CXExpr,
    rhs: &mut CXExpr
) -> Option<CXType> {
    let CXTypeKind::MemoryReference(inner) = &lhs_type.kind else {
        log_error!("TYPE ERROR: Method call on {lhs_type} without a memory reference type");
    };

    let Some(type_name) = inner.get_name() else {
        log_error!("TYPE ERROR: Method call on {lhs_type} without a type name");
    };

    let prototype = match &rhs.kind {
        CXExprKind::Identifier(name) => {
            let name = CXFunctionIdentifier::MemberFunction {
                function_name: name.clone(),
                object: CXObjectIdentifier::Standard(CXIdent::from(type_name)),
            }.as_string();

            let Some(prototype) = env.fn_map.get(&name).cloned() else {
                log_error!("TYPE ERROR: Method call on {lhs_type} with invalid method name {name}");
            };

            prototype
        },
        CXExprKind::TemplatedIdentifier { name: fn_name, template_input } => {
            let name = CXFunctionIdentifier::MemberFunction {
                function_name: fn_name.clone(),
                object: CXObjectIdentifier::Standard(CXIdent::from(type_name)),
            }.as_string();
            
            let input = contextualize_template_args(
                env.type_map,
                template_input
            )?;

            let Some(prototype) = env.fn_map
                .get_template(env.type_map, &name, input) else {
                log_error!("TYPE ERROR: Method call on {lhs_type} with invalid method name {name}");
            };

            prototype
        },

        _ => log_error!("TYPE ERROR: Method call on {lhs_type} with invalid method name")
    };
    
    Some(CXTypeKind::Function { prototype: Box::new(prototype) }.into())
}

pub fn typecheck_access(
    env: &mut TypeEnvironment,
    lhs: &mut CXExpr,
    rhs: &mut CXExpr
) -> Option<CXType> {
    let mut lhs_type = coerce_value(env, lhs).unwrap().clone();
    
    if lhs_type.is_pointer() {
        let lhs_temp = std::mem::take(lhs);
        let start_index = lhs_temp.start_index;
        let end_index = lhs_temp.end_index;
        
        *lhs =
            CXExprKind::UnOp {
                operator: CXUnOp::Dereference,
                operand: Box::new(lhs_temp)
            }.into_expr(start_index, end_index);

        lhs_type = type_check_traverse(env, lhs)?.clone();
    }
    
    if let CXExprKind::Identifier(accessor) = &rhs.kind {
        let CXTypeKind::MemoryReference(inner) = &lhs_type.kind else {
            log_error!("TYPE ERROR: Cannot access field on {accessor} type {lhs_type}\n\
                    Variable returned a type of {}, expected a memory reference type", lhs_type.kind);
        };

        let access_type = match &inner.kind {
            CXTypeKind::Union { fields, .. } |
            CXTypeKind::Structured { fields, .. } => {
                fields.iter()
                    .find(|(name, _)| name == accessor.as_str())
                    .map(|(_, field_type)| {
                        CXType::new(
                            0,
                            CXTypeKind::MemoryReference(Box::new(field_type.clone()))
                        )
                    })
            },

            _ => log_error!("TYPE ERROR: Cannot access field on {accessor} type {lhs_type}")
        };

        if access_type.is_some() {
            return access_type;
        }
    }

    typecheck_member_function(env, &lhs_type, lhs, rhs)
}

pub fn typecheck_method_call(env: &mut TypeEnvironment, prototype: &CXFunctionPrototype, rhs: &mut CXExpr) -> Option<CXType> {
    let mut args = comma_separated_mut(rhs);
    
    if args.len() != prototype.params.len() && !prototype.var_args {
        log_error!("TYPE ERROR: Method {} expects {} arguments, found {}", prototype.name.as_string(), prototype.params.len(), args.len());
    }

    for (arg, expected_type) in
        args
            .iter_mut()
            .zip(prototype.params.iter())
    {
        implicit_coerce(env, arg, expected_type._type.clone())?;
    }

    for arg in args.into_iter().skip(prototype.params.len()) {
        let va_type = coerce_value(env, arg)?;

        match &va_type.kind {
            CXTypeKind::PointerTo { .. } => {
                // Pointer types are already compatible with varargs, no need to cast
            },

            CXTypeKind::Integer { bytes, signed } => {
                if *bytes != 8 {
                    let to_type = CXTypeKind::Integer { bytes: 8, signed: *signed }.into();
                    implicit_cast(env, arg, &va_type, &to_type)?;
                }
            },

            CXTypeKind::Float { bytes } => {
                if *bytes != 8 {
                    let to_type = CXTypeKind::Float { bytes: 8 }.into();
                    implicit_cast(env, arg, &va_type, &to_type)?;
                }
            },

            CXTypeKind::Bool => {
                let to_type = CXTypeKind::Integer { bytes: 8, signed: false }.into();
                implicit_cast(env, arg, &va_type, &to_type)?;
            },

            _ => log_error!("TYPE ERROR: Cannot coerce value {} for varargs, expected intrinsic type or pointer!", arg),
        }
    }

    Some(prototype.return_type.clone()) 
}