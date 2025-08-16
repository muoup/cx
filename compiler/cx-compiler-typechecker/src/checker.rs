use cx_compiler_ast::parse::operators::{comma_separated_mut};
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXExprKind, CXGlobalConstant, CXGlobalVariable, CXUnOp};
use cx_data_ast::parse::type_mapping::{contextualize_template_args, contextualize_type};
use cx_data_ast::parse::value_type::{same_type, CXTypeKind, CXType};
use cx_data_ast::preparse::pp_type::CX_CONST;
use cx_util::{expr_error_log, log_error};
use crate::struct_typechecking::{typecheck_access, typecheck_method_call};
use crate::casting::{alg_bin_op_coercion, explicit_cast, implicit_cast};
use crate::structured_initialization::coerce_initializer_list;
use crate::TypeEnvironment;

pub(crate) fn type_check_traverse(env: &mut TypeEnvironment, expr: &mut CXExpr) -> Option<CXType> {
    if let Some(_type) = env.typecheck_data.expr_type_test(expr.uuid) {
        return Some(_type.clone());
    }
    
    let _type = type_check_inner(env, expr)
        .unwrap_or_else(|| panic!("Expression {expr} failed to type check"));
    
    if _type.is_strong_pointer() {
        env.typecheck_data.set_deferring_function(
            env.current_prototype.as_ref().unwrap().name.as_string()
        );
    } 

    env.typecheck_data.insert(expr, _type.clone()).cloned()
}

fn type_check_inner(env: &mut TypeEnvironment, expr: &mut CXExpr) -> Option<CXType> {
    match &mut expr.kind {
        CXExprKind::TemplatedFnIdent { fn_name, template_input } => {
            if !env.fn_map.has_template(fn_name.as_str()) {
                log_error!("TYPE ERROR: Unknown template function {fn_name}");
            }
            
            let contextualized_input = contextualize_template_args(env.type_map, template_input)?;
            
            let Some(template) = env.function_template_prototype(fn_name.as_str(), contextualized_input) else {
                log_error!("TYPE ERROR: Could not generate template for {fn_name} with input {template_input:?}");
            };
            
            Some(
                CXTypeKind::Function { prototype: Box::new(template.clone()) }
                    .into()
            )
        },
        
        CXExprKind::Block { exprs, value } => {
            for expr in exprs {
                type_check_traverse(env, expr)?;
            }

            if let Some(value) = value {
                return type_check_traverse(env, value.as_mut());
            }

            Some(CXType::unit())
        },

        CXExprKind::UnOp { operator, operand } => {
            match operator {
                CXUnOp::BNot |
                CXUnOp::Negative => { 
                    let mut _type = coerce_value(env, operand.as_mut())?;
                    
                    if _type.is_integer() {
                        implicit_coerce(env, operand, CXTypeKind::Integer { signed: true, bytes: 8 }.into())?;
                        _type = type_check_traverse(env, operand.as_mut())?;
                    }
                    
                    Some(_type)
                },
                
                CXUnOp::LNot => {
                    let mut _type = coerce_value(env, operand.as_mut())?;
                    
                    if !_type.is_integer() {
                        implicit_coerce(env, operand, CXTypeKind::Integer { signed: true, bytes: 8 }.into())?;
                        _type = type_check_traverse(env, operand.as_mut())?;
                    }
                    
                    Some(
                        CXType::new(
                            _type.specifiers,
                            CXTypeKind::Integer { bytes: 0, signed: false }
                        )
                    )
                },
                
                CXUnOp::AddressOf => {
                    let operand_type = type_check_traverse(env, operand.as_mut())?.clone();
                    
                    match operand_type.kind {
                        CXTypeKind::MemoryReference(inner) => Some(inner.clone().pointer_to()),
                        CXTypeKind::Function { .. } => coerce_value(env, operand.as_mut()),

                        _ => log_error!("TYPE ERROR: Cannot take address of expr: {operand_type}"),
                    }
                },
                CXUnOp::Dereference => {
                    let operand_type = coerce_value(env, operand.as_mut())?;
                    
                    let CXTypeKind::PointerTo { inner_type: inner, .. } = operand_type.kind else {
                        log_error!("TYPE ERROR: Dereference operator can only be applied to pointers, found {operand} of type {operand_type}");
                    };
                    
                    Some(
                        CXType::new(
                            0,
                            CXTypeKind::MemoryReference(inner)
                        )
                    )
                },
                CXUnOp::PostIncrement(_) |
                CXUnOp::PreIncrement(_) => {
                    let operand_type = type_check_traverse(env, operand.as_mut())?.clone();
                    
                    let CXTypeKind::MemoryReference(inner) = &operand_type.kind else {
                        log_error!("TYPE ERROR: Increment operator can only be applied to memory references, found: {operand}");
                    };
                    
                    match &inner.kind {
                        CXTypeKind::Integer { .. } |
                        CXTypeKind::PointerTo { .. } => {},

                        _ => log_error!("TYPE ERROR: Increment operator can only be applied to integers or pointers, found: {inner}"),
                    };

                    Some(*inner.clone())
                },

                CXUnOp::ExplicitCast (to_type) => {
                    let mut operand_type = coerce_value(env, operand.as_mut())?;
                    let contextualized_type = contextualize_type(env.type_map, to_type)?;
                    
                    let Some(_) = explicit_cast(env, operand, &mut operand_type, &contextualized_type) else {
                        expr_error_log!(env.tokens, expr.start_index, expr.end_index,
                            "TYPE ERROR: Invalid cast from {operand_type} to {to_type}");
                    };

                    Some(contextualized_type)
                },

                _ => todo!("CXUnOp {operator:?} not implemented"),
            }
        },

        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::Assign(op) } => {
            if op.is_some() { todo!("Compound assignment") }
            
            let lhs_type = type_check_traverse(env, lhs)?.clone();
            
            let CXTypeKind::MemoryReference(lhs_type) = &lhs_type.kind else {
                log_error!("TYPE ERROR: Assignment operator can only be applied to memory references, found: {lhs_type}");
            };

            if lhs_type.as_ref().get_specifier(CX_CONST) {
                log_error!("TYPE ERROR: Assignment operator cannot be applied to const variables");
            }
            
            implicit_coerce(env, rhs, lhs_type.as_ref().clone())?;

            Some(*lhs_type.clone())
        },

        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::Access } =>
            typecheck_access(env, lhs.as_mut(), rhs.as_mut()),

        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::MethodCall } => {
            let mut lhs_type = coerce_mem_ref(env, lhs.as_mut())?;

            if let CXTypeKind::PointerTo { inner_type: inner, .. } = &lhs_type.kind {
                lhs_type = *inner.clone();
            }
            
            let CXTypeKind::Function { prototype } = &lhs_type.kind else {
                log_error!("TYPE ERROR: Method call operator can only be applied to functions, found: {lhs} of type {lhs_type}");
            };
            
            typecheck_method_call(env, prototype.as_ref(), rhs.as_mut())
        },

        CXExprKind::BinOp { lhs, rhs, op } =>
            alg_bin_op_coercion(env, op.clone(), lhs, rhs)
                .or_else(|| {
                    let l_type = type_check_traverse(env, lhs)?.clone();
                    let r_type = type_check_traverse(env, rhs)?.clone();
                    
                    expr_error_log!(env.tokens, expr.start_index, expr.end_index, "TYPE ERROR: Invalid binary operation {op} for {l_type} and {r_type}")
                }),

        CXExprKind::VarDeclaration { name, type_ } => {
            if let CXTypeKind::VariableLengthArray { _type, size } = &mut type_.kind {
                implicit_coerce(env, size.as_mut(), CXTypeKind::Integer { bytes: 8, signed: false }.into())?;
            };
            
            env.symbol_table.insert(name.as_string(), type_.clone());
            
            type_check_inner(
                env,
                &mut CXExpr {
                    uuid: expr.uuid,
                    kind: CXExprKind::Identifier(name.clone()),
                    start_index: 0,
                    end_index: 0
                }
            )
        },

        CXExprKind::Identifier(name) => {
            if let Some(record) = env.symbol_table.get(name.as_str()).cloned() {
                return match &record.kind {
                    // Array variables are themselves memory aliases, so wrapping
                    // them in a memory alias ends up adding an extra load operation
                    // when using them
                    CXTypeKind::VariableLengthArray { .. } |
                    CXTypeKind::Array { .. } => Some(record),
                    
                    _ => {
                        Some(
                            CXType::new(
                                record.specifiers,
                                CXTypeKind::MemoryReference(Box::new(record))
                            )
                        )
                    },
                };
            };

            if let Some(func) = env.fn_map.get(name.as_str()) {
                return Some(
                    CXTypeKind::Function { prototype: Box::new(func.clone()) }.into()
                );
            };

            if let Some(glob) = env.global_variables.get(name.as_str()) {
                return match glob {
                    CXGlobalVariable::GlobalConstant { constant: global_constant, .. } => {
                        *expr = match global_constant {
                            CXGlobalConstant::Int(i) => {
                                CXExprKind::IntLiteral {
                                    bytes: 4,
                                    val: *i as i64,
                                }.into_expr(expr.start_index, expr.end_index)
                            },
                            
                            _ => log_error!("TYPE ERROR: Global constant expected, found: {glob:?}"),
                        };
                        
                        Some(CXTypeKind::Integer { bytes: 4, signed: true }.into())
                    }
                };
            }

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
                env.type_map.get("char")
                    .expect("INTERNAL ERROR: Expected char type to be present in type map")
                    .clone()
                    .pointer_to()
                    .add_specifier(CX_CONST)
            )
        },

        CXExprKind::If { condition, then_branch, else_branch } => {
            let condition_type = coerce_value(env, condition)?;
            
            if !condition_type.is_integer() {
                implicit_coerce(env, condition, CXTypeKind::Integer { signed: true, bytes: 8 }.into())?;
            }

            type_check_traverse(env, then_branch)?;
            if let Some(else_branch) = else_branch {
                type_check_traverse(env, else_branch)?;
            }

            Some(CXType::unit())
        },
        
        CXExprKind::Switch { condition, block, .. } => {
            let condition_type = coerce_value(env, condition)?;
            
            if !condition_type.is_integer() {
                implicit_coerce(env, condition, CXTypeKind::Integer { signed: true, bytes: 8 }.into())?;
            }

            for expr in block {
                type_check_traverse(env, expr);
            }

            Some(CXType::unit())
        },

        CXExprKind::Return { value } => {
            if let Some(value) = value {
                let return_type = env.current_prototype.as_ref()?.return_type.clone();
                coerce_return_value(env, value, &return_type)?;
            } else if !env.current_prototype.as_ref()?.return_type.is_unit() {
                log_error!("TYPE ERROR: Function with empty return in non-void context");
            }

            Some(CXType::unit())
        },
        
        CXExprKind::Defer { expr } => {
            type_check_traverse(env, expr)?;
            
            let fn_name = env.current_prototype.clone()?.name;
            env.typecheck_data.set_deferring_function(fn_name.as_string());
            
            Some(CXType::unit())
        },
        
        CXExprKind::ImplicitCast { to_type, .. } => Some(to_type.clone()),
        CXExprKind::ImplicitLoad { loaded_type, .. } => Some(loaded_type.clone()),
        CXExprKind::GetFunctionAddr { func_sig, .. } => Some(func_sig.clone().pointer_to()),

        CXExprKind::Move { expr } => {
            let expr_type = type_check_traverse(env, expr)?.clone();
            
            let CXTypeKind::MemoryReference(inner) = &expr_type.kind else {
                log_error!("TYPE ERROR: Move operator can only be applied to memory references, found: {expr_type}");
            };
            
            if !inner.is_strong_pointer() {
                log_error!("TYPE ERROR: Move operator can only be applied to lvalue strong pointers, found: {expr_type}");
            };
            
            Some(*inner.clone())
        },
        
        CXExprKind::For { init, condition, increment, body } => {
            type_check_traverse(env, init)?;
            let condition_type = coerce_value(env, condition)?.clone();
            
            if !condition_type.is_integer() {
                implicit_coerce(env, condition, CXTypeKind::Integer { signed: true, bytes: 8 }.into())?;
            }
            
            type_check_traverse(env, increment)?;
            type_check_traverse(env, body)?;

            Some(CXType::unit())
        },

        CXExprKind::While { condition, body, .. } => {
            let condition_type = type_check_traverse(env, condition)?.clone();

            if !condition_type.is_integer() {
                implicit_coerce(env, condition, CXTypeKind::Integer { signed: true, bytes: 8 }.into())?;
            }

            type_check_traverse(env, body)?;

            Some(CXType::unit())
        },

        CXExprKind::SizeOf { expr } => {
            coerce_value(env, expr)?;
            
            Some(CXTypeKind::Integer { bytes: 8, signed: false }.into())
        },
        
        CXExprKind::New { _type, array_length } => {
            if let Some(array_length) = array_length {
                implicit_coerce(env, array_length, CXTypeKind::Integer { signed: true, bytes: 8 }.into())?;
            }

            Some(
                CXType::new(
                    _type.specifiers,
                    CXTypeKind::StrongPointer {
                        inner_type: Box::new(_type.clone()),
                        is_array: array_length.is_some()
                    }
                )
            )
        },
        
        CXExprKind::InvokeDestructor { .. } |
        CXExprKind::InitializerList { .. } =>
            Some(CXType::unit()),
        
        CXExprKind::Unit |
        CXExprKind::Break |
        CXExprKind::Continue => Some(CXType::unit()),
        
        CXExprKind::Taken => panic!("INTERNAL ERROR: Unexpected Taken expression in type checker"),
    }
}

pub(crate) fn implicit_coerce(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
    to_type: CXType
) -> Option<()> {
    if matches!(expr.kind, CXExprKind::InitializerList { .. }) {
        coerce_initializer_list(env, expr, &to_type)?;
    }
    
    let from_type = coerce_value(env, expr)?;

    if same_type(env.type_map, &from_type, &to_type) {
        return Some(());
    }

    let Some(()) = implicit_cast(env, expr, &from_type, &to_type)? else {
        expr_error_log!(env.tokens, expr.start_index, expr.end_index, "TYPE ERROR: Cannot implicitly cast {from_type} to {to_type}");
    };

    Some(())
}

pub(crate) fn coerce_mem_ref(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
) -> Option<CXType> {
    let expr_type = type_check_traverse(env, expr)?.clone();

    let CXTypeKind::MemoryReference(inner) = &expr_type.kind else {
        return Some(expr_type);
    };
    
    match &inner.kind {
        CXTypeKind::Union { .. } |
        CXTypeKind::Structured { .. } => {},
        
        _ => {
            let expr_temp = std::mem::take(expr);
            let start_index = expr_temp.start_index;
            let end_index = expr_temp.end_index;
            
            *expr = CXExprKind::ImplicitLoad {
                expr: Box::new(expr_temp),
                loaded_type: inner.as_ref().clone()
            }.into_expr(start_index, end_index);
        }
    }

    type_check_traverse(env, expr)
}

pub(crate) fn coerce_return_value(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
    return_type: &CXType,
) -> Option<()> {
    let _type = type_check_traverse(env, expr)?.clone();
    
    let rvalue = _type.is_memory_reference();
    let value_type = coerce_mem_ref(env, expr)?;
    
    match value_type.kind {
        // The memory alias is not traditionally loadable (i.e. a struct), so it may be returned as-is
        CXTypeKind::MemoryReference(_) => (),
        
        // In most cases, a strong pointer is implicitly converted to a regular pointer for standard
        // C semantics, however in the case of returning, it is import to maintain the strongness
        // as the caller expects to own the pointer afterward. In the case that this value is
        // a lvalue, the callee is expected to explicitly move the pointer, thus making it an
        // rvalue.
        CXTypeKind::StrongPointer { .. } if rvalue => (),

        _ => implicit_coerce(env, expr, return_type.clone())?
    }
    
    Some(())
}

fn modify_load(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
    load_instead: CXType
) -> Option<CXType> {
    let CXExprKind::ImplicitLoad { loaded_type, .. } = &mut expr.kind else {
        log_error!("INTERNAL ERROR: Expected an implicit load expression, found: {expr}");
    };
    
    *loaded_type = load_instead.clone();
    env.typecheck_data.insert(expr, load_instead).cloned()
}

pub(crate) fn coerce_value(
    env: &mut TypeEnvironment,
    expr: &mut CXExpr,
) -> Option<CXType> {
    let expr_type = type_check_traverse(env, expr)?.clone();
    
    let Some(inner) = expr_type.mem_ref_inner() else {
        return Some(expr_type);
    };
    
    match &inner.kind {
        CXTypeKind::Structured { .. } |
        CXTypeKind::Union { .. } => return Some(expr_type),
        
        _ => (),
    };
    
    let expr_type = coerce_mem_ref(env, expr)?;

    match expr_type.kind {
        // If used in a value context, any type of array is used as a pointer to its first element
        CXTypeKind::VariableLengthArray { _type, .. } |
        CXTypeKind::Array { inner_type: _type, .. } =>
            modify_load(env, expr, _type.clone().pointer_to().add_specifier(expr_type.specifiers)),

        // If the value is an already owned lvalue strong pointer,
        // it should be semantically equivalent to a plain [weak] pointer
        CXTypeKind::StrongPointer { inner_type: inner, .. } => 
            modify_load(
                env, expr,
                CXType::new(
                    expr_type.specifiers,
                    CXTypeKind::PointerTo {
                        inner_type: inner.clone(),
                        
                        sizeless_array: false,
                        weak: false,
                        nullable: true,
                    }
                )
            ),
        
        CXTypeKind::Function { .. } => {
            let expr_temp = std::mem::take(expr);
            let start_index = expr_temp.start_index;
            let end_index = expr_temp.end_index;
            *expr = CXExprKind::GetFunctionAddr {
                func_name: Box::new(expr_temp),
                func_sig: expr_type.clone()
            }.into_expr(start_index, end_index);
            type_check_traverse(env, expr)
        },
        
        _ => Some(expr_type)
    }
}
