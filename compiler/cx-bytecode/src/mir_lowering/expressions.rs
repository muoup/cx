//! Expression tree lowering from MIRExpression to bytecode
//!
//! This module handles lowering of MIRExpression (AST-style IR) to bytecode.

use cx_bytecode_data::{
    types::{BCIntegerType, BCType, BCTypeKind},
    BCInstructionKind, BCIntBinOp, BCPtrBinOp, BCValue,
};
use cx_typechecker_data::mir::{
    expression::{MIRExpression, MIRExpressionKind, StructInitialization},
    program::MIRFunction,
    types::MIRTypeKind,
};
use cx_util::CXResult;

use crate::{builder::BCBuilder, mir_lowering::deconstructors::{allocate_liveness_variable, needs_deconstruction}};

use super::binary_ops::{lower_binary_op, lower_unary_op};
use super::coercion::lower_type_conversion;
use super::control_flow::{
    lower_cswitch, lower_for, lower_if, lower_match, lower_return, lower_while,
};
use super::tagged_union::{
    get_tagged_union_tag, lower_construct_tagged_union, lower_tagged_union_get,
    lower_tagged_union_set,
};

pub fn lower_expression(builder: &mut BCBuilder, expr: &MIRExpression) -> CXResult<BCValue> {
    match &expr.kind {
        MIRExpressionKind::BoolLiteral(value) => Ok(BCValue::IntImmediate {
            val: if *value { 1 } else { 0 },
            _type: BCIntegerType::I1,
        }),

        MIRExpressionKind::IntLiteral(val, _type, _signed) => {
            let bc_type = builder.convert_integer_type(_type);
            Ok(BCValue::IntImmediate {
                val: *val,
                _type: bc_type,
            })
        }

        MIRExpressionKind::FloatLiteral(val, _type) => {
            let bc_type = builder.convert_float_type(_type);
            Ok(BCValue::FloatImmediate {
                val: *val,
                _type: bc_type,
            })
        }

        MIRExpressionKind::Unit => Ok(BCValue::NULL),
        MIRExpressionKind::Null => Ok(BCValue::NULL),

        MIRExpressionKind::Variable(name) => {
            if let Some(local_value) = builder.get_symbol(name) {
                return Ok(local_value);
            }

            if let Some(global_value) = builder.get_global_symbol(name.as_str()) {
                return Ok(global_value);
            }

            unreachable!("Variable '{}' not found in symbol table", name);
        }

        MIRExpressionKind::ContractVariable {
            name,
            parent_function,
        } => {
            // If we are currently in  the parent function, our contract variables are lvalues (the parameters)
            // therefore we need to load them
            if builder.current_function_name().unwrap() == parent_function.as_str() {
                let Some(parameter_index) = builder
                    .current_prototype()
                    .params
                    .iter()
                    .position(|p| p.name.as_ref().map(|s| s.as_str()) == Some(name.as_str()))
                else {
                    unreachable!(
                        "Contract variable '{}' not found in parameters of function '{}'",
                        name, parent_function
                    );
                };

                Ok(BCValue::ParameterRef(parameter_index as u32))
            } else {
                let Some(local_value) = builder.get_symbol(name) else {
                    unreachable!("Contract variable '{}' not found in symbol table", name);
                };

                // Otherwise, they are rvalues (the arguments passed to the contract)
                Ok(local_value)
            }
        }

        MIRExpressionKind::FunctionReference { .. } => {
            let MIRTypeKind::Function { prototype } = &expr._type.kind else {
                unreachable!("FunctionReference must have function type");
            };

            Ok(BCValue::FunctionRef(prototype.name.clone()))
        }

        // ===== Arithmetic & Logic =====
        MIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
            lower_binary_op(builder, lhs, rhs, op, &expr._type)
        }

        MIRExpressionKind::UnaryOperation { operand, op } => {
            lower_unary_op(builder, operand, op, &expr._type)
        }

        MIRExpressionKind::MemoryRead { source } => {
            let bc_source = lower_expression(builder, source)?;
            let bc_type = builder.convert_cx_type(&expr._type);

            builder.add_new_instruction(
                BCInstructionKind::Load {
                    memory: bc_source,
                    _type: bc_type,
                },
                builder.convert_cx_type(&expr._type),
                true,
            )
        }

        MIRExpressionKind::Typechange(inner) => lower_expression(builder, inner),

        MIRExpressionKind::CreateStackVariable { name, _type } => {
            let bc_type = builder.convert_cx_type(_type);

            let result = builder.add_new_instruction(
                BCInstructionKind::Allocate {
                    alignment: bc_type.alignment(),
                    _type: bc_type,
                },
                BCType::default_pointer(),
                true,
            )?;

            if let Some(name) = name {
                builder.insert_symbol(name.clone(), result.clone());
                
                if let BCValue::Register { register, .. } = &result {
                    if needs_deconstruction(builder, _type) {
                        let liveness = allocate_liveness_variable(builder)?;
                        let ptr_val = BCValue::Register {
                            register: register.clone(),
                            _type: BCType::default_pointer(),
                        };
                        
                        builder.add_liveness_mapping(name.to_string(), ptr_val, liveness, _type.clone());
                    }
                }
            }

            Ok(result)
        }

        MIRExpressionKind::MemoryWrite { target, value } => {
            let bc_target = lower_expression(builder, target)?;
            let bc_value = lower_expression(builder, value)?;
            let mir_value_type = &value._type;
            let bc_type = builder.convert_cx_type(mir_value_type);

            if mir_value_type.is_memory_resident() {
                builder.add_new_instruction(
                    BCInstructionKind::Memcpy {
                        dest: bc_target,
                        src: bc_value,
                        size: BCValue::IntImmediate {
                            val: bc_type.size() as i64,
                            _type: BCIntegerType::I64,
                        },
                        alignment: bc_type.alignment(),
                    },
                    BCType::unit(),
                    false,
                )
            } else {
                builder.add_new_instruction(
                    BCInstructionKind::Store {
                        memory: bc_target,
                        value: bc_value,
                        _type: bc_type,
                    },
                    BCType::unit(),
                    false,
                )
            }
        }

        MIRExpressionKind::Move { source } => lower_expression(builder, source),

        MIRExpressionKind::CopyRegion { source, _type } => {
            let new_region = builder.add_new_instruction(
                BCInstructionKind::Allocate {
                    alignment: builder.convert_cx_type(_type).alignment(),
                    _type: builder.convert_cx_type(_type),
                },
                BCType::default_pointer(),
                true,
            )?;

            let bc_source = lower_expression(builder, source)?;

            builder.add_new_instruction(
                BCInstructionKind::Memcpy {
                    dest: new_region.clone(),
                    src: bc_source.clone(),
                    size: BCValue::IntImmediate {
                        val: builder.convert_cx_type(_type).size() as i64,
                        _type: BCIntegerType::I64,
                    },
                    alignment: builder.convert_cx_type(_type).alignment(),
                },
                BCType::unit(),
                false,
            )?;

            Ok(new_region)
        }

        MIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => lower_if(
            builder,
            condition,
            then_branch,
            else_branch.as_deref(),
            &expr._type,
        ),

        MIRExpressionKind::While {
            condition,
            body,
            pre_eval,
        } => lower_while(builder, condition, body, *pre_eval),

        MIRExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => lower_for(builder, init, condition, increment, body),

        MIRExpressionKind::Return { value } => {
            let val = value
                .as_ref()
                .map(|v| lower_expression(builder, v))
                .transpose()?;
            lower_return(builder, val)
        }

        MIRExpressionKind::Block { statements } => {
            for statement in statements {
                lower_expression(builder, statement)?;
            }

            Ok(BCValue::NULL)
        }

        MIRExpressionKind::CallFunction {
            function,
            arguments,
        } => lower_call(builder, function, arguments, &expr._type),

        MIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => lower_type_conversion(builder, operand, *conversion, &expr._type),

        MIRExpressionKind::LifetimeStart {
            variable: _,
            _type: _,
        } => Ok(BCValue::NULL),
        MIRExpressionKind::LifetimeEnd {
            variable: _,
            _type: _,
        } => Ok(BCValue::NULL),
        MIRExpressionKind::LeakLifetime { expression } => lower_expression(builder, expression),

        MIRExpressionKind::StructFieldAccess {
            base,
            field_index,
            field_offset,
            struct_type,
        } => {
            let bc_base = lower_expression(builder, base)?;
            let bc_struct_type = builder.convert_cx_type(struct_type);

            builder.add_new_instruction(
                BCInstructionKind::StructAccess {
                    struct_: bc_base,
                    struct_type: bc_struct_type,
                    field_index: *field_index,
                    field_offset: *field_offset,
                },
                BCType::default_pointer(),
                true,
            )
        }

        MIRExpressionKind::UnionAliasAccess { base, .. } => lower_expression(builder, base),

        MIRExpressionKind::ArrayAccess {
            array,
            index,
            element_type,
        } => {
            let bc_array = lower_expression(builder, array)?;
            let bc_index = lower_expression(builder, index)?;
            let bc_element_type = builder.convert_cx_type(element_type);
            let element_size = bc_element_type.size() as u64;

            builder.add_new_instruction(
                BCInstructionKind::PointerBinOp {
                    op: BCPtrBinOp::ADD,
                    ptr_type: bc_element_type,
                    type_padded_size: element_size,
                    left: bc_array,
                    right: bc_index,
                },
                BCType::default_pointer(),
                true,
            )
        }

        MIRExpressionKind::PatternIs {
            lhs,
            sum_type,
            variant_index,
            inner_name,
        } => {
            let bc_lhs = lower_expression(builder, lhs)?;

            let alias = builder.add_new_instruction(
                BCInstructionKind::Alias {
                    value: bc_lhs.clone(),
                },
                BCType::default_pointer(),
                true,
            )?;

            builder.insert_symbol(inner_name.clone(), alias);
            let tag_ptr = get_tagged_union_tag(builder, bc_lhs, sum_type)?;
            let tag_value = builder.add_new_instruction(
                BCInstructionKind::Load {
                    memory: tag_ptr,
                    _type: BCType::from(BCTypeKind::Integer(BCIntegerType::I8)),
                },
                BCType::from(BCTypeKind::Integer(BCIntegerType::I8)),
                true,
            )?;

            let comparison = builder.add_new_instruction(
                BCInstructionKind::IntegerBinOp {
                    op: BCIntBinOp::EQ,
                    left: tag_value,
                    right: BCValue::IntImmediate {
                        val: *variant_index as i64,
                        _type: BCIntegerType::I8,
                    },
                },
                BCType::bool(),
                true,
            )?;

            Ok(comparison)
        }

        MIRExpressionKind::TaggedUnionTag { value, sum_type } => {
            let bc_value = lower_expression(builder, value)?;
            get_tagged_union_tag(builder, bc_value, &sum_type)
        }

        MIRExpressionKind::TaggedUnionGet { value, .. } => lower_tagged_union_get(builder, value),

        MIRExpressionKind::TaggedUnionSet {
            target,
            variant_index,
            inner_value,
            sum_type,
        } => lower_tagged_union_set(builder, target, *variant_index, inner_value, sum_type),

        MIRExpressionKind::ConstructTaggedUnion {
            variant_index,
            value,
            sum_type,
        } => lower_construct_tagged_union(builder, *variant_index, value, sum_type),

        // ===== Initializers =====
        MIRExpressionKind::ArrayInitializer {
            elements,
            element_type,
        } => lower_array_initializer(builder, elements, element_type),

        MIRExpressionKind::StructInitializer {
            initializations,
            struct_type,
        } => lower_struct_initializer(builder, initializations, struct_type),

        MIRExpressionKind::Break { scope_depth } => {
            let current_depth = builder.scope_depth();
            for i in ((scope_depth + 1)..=current_depth).rev() {
                builder.deconstruct_at_depth(i)?;
            }

            let Some(to) = builder.get_break_target().cloned() else {
                unreachable!("Break used outside of loop or switch context");
            };

            builder.add_new_instruction(
                BCInstructionKind::Jump { target: to },
                BCType::unit(),
                false,
            )
        }

        MIRExpressionKind::Continue { scope_depth } => {
            for i in ((scope_depth + 1)..=builder.scope_depth()).rev() {
                builder.deconstruct_at_depth(i)?;
            }

            let Some(to) = builder.get_continue_block().cloned() else {
                unreachable!("Continue used outside of loop context");
            };

            builder.add_new_instruction(
                BCInstructionKind::Jump { target: to },
                BCType::unit(),
                false,
            )
        }

        MIRExpressionKind::CSwitch {
            condition,
            cases,
            default,
        } => lower_cswitch(builder, condition, cases, default.as_deref()),

        MIRExpressionKind::Match {
            condition,
            arms,
            default,
        } => lower_match(builder, condition, arms, default.as_deref()),

        MIRExpressionKind::Defer { .. } => {
            todo!("Defer lowering - to be implemented")
        }
    }
}

fn lower_call(
    builder: &mut BCBuilder,
    function: &MIRExpression,
    arguments: &[MIRExpression],
    result_type: &cx_typechecker_data::mir::types::MIRType,
) -> CXResult<BCValue> {
    let return_type = builder.convert_cx_type(result_type);

    let fn_val = lower_expression(builder, function)?;
    let mut args = arguments
        .iter()
        .map(|arg| lower_expression(builder, arg))
        .collect::<CXResult<Vec<BCValue>>>()?;

    let MIRTypeKind::Function { prototype } = &function._type.kind else {
        unreachable!("CallFunction must have function type");
    };

    let bc_prototype = builder.convert_cx_prototype(prototype);

    if let Some(precondition) = &prototype.contract.precondition {
        builder.push_scope(None, None);

        for (arg_expr, param) in args.iter().cloned().zip(prototype.params.iter()) {
            if let Some(name) = &param.name {
                builder.insert_symbol(name.clone(), arg_expr);
            }
        }

        lower_contract_assertion(builder, precondition, "precondition violation")?;

        builder.pop_scope()?;
    }

    // Capture args for postcondition binding before adding return buffer
    let args_cloned = if prototype.contract.postcondition.is_some() {
        args.clone()
    } else {
        vec![]
    };

    let return_buffer = if prototype.return_type.is_memory_resident() {
        let buffer = builder.add_new_instruction(
            BCInstructionKind::Allocate {
                alignment: return_type.alignment(),
                _type: return_type.clone(),
            },
            BCType::default_pointer(),
            true,
        )?;
        args.insert(0, buffer.clone());
        Some(buffer)
    } else {
        None
    };

    let value = if let BCValue::FunctionRef(_) = &fn_val {
        builder.add_new_instruction(
            BCInstructionKind::DirectCall {
                args,
                method_sig: bc_prototype,
            },
            if return_buffer.is_some() {
                BCType::default_pointer()
            } else {
                return_type
            },
            true,
        )?
    } else {
        builder.add_new_instruction(
            BCInstructionKind::IndirectCall {
                method_sig: bc_prototype,
                func_ptr: fn_val,
                args,
            },
            if return_buffer.is_some() {
                BCType::default_pointer()
            } else {
                return_type
            },
            true,
        )?
    };

    if let Some((ret_name, postcondition)) = &prototype.contract.postcondition {
        builder.push_scope(None, None);

        for (arg_expr, param) in args_cloned.into_iter().zip(prototype.params.iter()) {
            if let Some(name) = &param.name {
                builder.insert_symbol(name.clone(), arg_expr);
            }
        }

        if let Some(ret_name) = ret_name {
            builder.insert_symbol(ret_name.clone(), value.clone());
        }

        // TODO: Emit postcondition assumption after call (currently a no-op)
        // In the future, this could be used for verification or optimization hints

        // For now for the sake of testing, we will just generate the inner expression so that it
        // is type-checked and any side-effects are captured
        lower_expression(builder, postcondition)?;

        builder.pop_scope()?;
    }

    Ok(value)
}

pub(crate) fn lower_contract_assertion(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    message: &str,
) -> CXResult<()> {
    let condition_val = lower_expression(builder, condition)?;
    let message_global = builder.create_static_string(message.to_string());

    let Some(assert_prototype) = builder.get_prototype("__compiler_assert").cloned() else {
        return cx_util::CXError::create_result(
            "Function contract used but __compiler_assert not found. Ensure std::intrinsic::assertion is imported.".to_string()
        );
    };

    builder.add_new_instruction(
        BCInstructionKind::DirectCall {
            args: vec![condition_val, message_global],
            method_sig: assert_prototype,
        },
        BCType::unit(),
        false,
    )?;

    Ok(())
}

fn lower_array_initializer(
    builder: &mut BCBuilder,
    elements: &[MIRExpression],
    element_type: &cx_typechecker_data::mir::types::MIRType,
) -> CXResult<BCValue> {
    let bc_element_type = builder.convert_cx_type(element_type);
    let element_size = bc_element_type.size() as u64;

    let array_type = BCType::from(BCTypeKind::Array {
        element: Box::new(bc_element_type.clone()),
        size: elements.len(),
    });

    let allocation = builder.add_new_instruction(
        BCInstructionKind::Allocate {
            alignment: array_type.alignment(),
            _type: array_type,
        },
        BCType::default_pointer(),
        true,
    )?;

    for (i, elem) in elements.iter().enumerate() {
        let bc_elem = lower_expression(builder, elem)?;
        let elem_type = builder.get_value_type(&bc_elem);

        let elem_addr = builder.add_new_instruction(
            BCInstructionKind::PointerBinOp {
                op: BCPtrBinOp::ADD,
                ptr_type: bc_element_type.clone(),
                type_padded_size: element_size,
                left: allocation.clone(),
                right: BCValue::IntImmediate {
                    val: i as i64,
                    _type: BCIntegerType::I64,
                },
            },
            BCType::default_pointer(),
            true,
        )?;

        // Check if bc_elem is a pointer to an in-memory aggregate (struct/union/array)
        // If so, we need to use Memcpy to copy the actual bytes, not Store the pointer
        let bc_elem_type_for_memcpy = builder.convert_cx_type(element_type);
        if element_type.is_memory_resident() {
            builder.add_new_instruction(
                BCInstructionKind::Memcpy {
                    dest: elem_addr,
                    src: bc_elem,
                    size: BCValue::IntImmediate {
                        val: bc_elem_type_for_memcpy.size() as i64,
                        _type: BCIntegerType::I64,
                    },
                    alignment: bc_elem_type_for_memcpy.alignment(),
                },
                BCType::unit(),
                false,
            )?;
        } else {
            builder.add_new_instruction(
                BCInstructionKind::Store {
                    memory: elem_addr,
                    value: bc_elem,
                    _type: elem_type,
                },
                BCType::unit(),
                false,
            )?;
        }
    }

    Ok(allocation)
}

fn lower_struct_initializer(
    builder: &mut BCBuilder,
    initializations: &[StructInitialization],
    struct_type: &cx_typechecker_data::mir::types::MIRType,
) -> CXResult<BCValue> {
    let bc_struct_type = builder.convert_cx_type(struct_type);

    let allocation = builder.add_new_instruction(
        BCInstructionKind::Allocate {
            alignment: bc_struct_type.alignment(),
            _type: bc_struct_type.clone(),
        },
        BCType::default_pointer(),
        true,
    )?;

    for initialization in initializations {
        let bc_value = lower_expression(builder, &initialization.value)?;
        let mir_field_type = &initialization.value._type;
        let bc_field_type = builder.convert_cx_type(mir_field_type);

        let field_addr = builder.add_new_instruction(
            BCInstructionKind::StructAccess {
                struct_: allocation.clone(),
                struct_type: bc_struct_type.clone(),
                field_index: initialization.field_index,
                field_offset: initialization.field_offset,
            },
            BCType::default_pointer(),
            true,
        )?;

        if mir_field_type.is_memory_resident() {
            builder.add_new_instruction(
                BCInstructionKind::Memcpy {
                    dest: field_addr,
                    src: bc_value,
                    size: BCValue::IntImmediate {
                        val: bc_field_type.size() as i64,
                        _type: BCIntegerType::I64,
                    },
                    alignment: bc_field_type.alignment(),
                },
                BCType::unit(),
                false,
            )?;
        } else {
            builder.add_new_instruction(
                BCInstructionKind::Store {
                    memory: field_addr,
                    value: bc_value,
                    _type: bc_field_type,
                },
                BCType::unit(),
                false,
            )?;
        }
    }

    Ok(allocation)
}

/// Generate bytecode for an MIR function
pub fn lower_function(builder: &mut BCBuilder, mir_fn: &MIRFunction) -> CXResult<()> {
    let return_buffer_size = if mir_fn.prototype.return_type.is_memory_resident() {
        Some(mir_fn.prototype.return_type.type_size())
    } else {
        None
    };

    builder.new_function(mir_fn.prototype.clone(), return_buffer_size);
    assert_eq!(builder.scope_depth(), 1);

    let entry_block = builder.create_block(Some("entry"));
    builder.set_current_block(entry_block);

    let has_return_buffer = mir_fn.prototype.return_type.is_memory_resident();
    let param_offset = if has_return_buffer { 1 } else { 0 };

    for (i, param) in mir_fn.prototype.params.iter().enumerate() {
        if let Some(name) = &param.name {
            let bc_param_type = builder.convert_cx_type(&param._type);

            if !param._type.is_memory_resident() {
                let alloc = builder.add_new_instruction(
                    BCInstructionKind::Allocate {
                        alignment: bc_param_type.alignment(),
                        _type: bc_param_type.clone(),
                    },
                    BCType::default_pointer(),
                    true,
                )?;
                builder.add_new_instruction(
                    BCInstructionKind::Store {
                        memory: alloc.clone(),
                        value: BCValue::ParameterRef((i + param_offset) as u32),
                        _type: bc_param_type,
                    },
                    BCType::unit(),
                    false,
                )?;
                builder.insert_symbol(name.clone(), alloc);
            } else {
                builder.insert_symbol(
                    name.clone(),
                    BCValue::ParameterRef((i + param_offset) as u32),
                );
            }
        }
    }
    
    assert_eq!(
        builder.scope_depth(),
        1,
        "Scope should be at function base after parameter insertion"
    );

    lower_expression(builder, &mir_fn.body)?;

    assert_eq!(
        builder.scope_depth(),
        1,
        "Scope should be at function base before finishing function"
    );

    if !matches!(
        builder.current_block_last_inst().map(|i| &i.kind),
        Some(BCInstructionKind::Return { .. })
    ) {
        if mir_fn.prototype.name.as_str() == "main" {
            lower_return(
                builder,
                Some(BCValue::IntImmediate {
                    val: 0,
                    _type: BCIntegerType::I32,
                }),
            )?;
        } else if mir_fn.prototype.return_type.is_unit() {
            lower_return(
                builder,
                None,
            )?;
        } else {
            unreachable!(
                "Function '{}' missing return statement",
                mir_fn.prototype.name
            );
        }
    }

    builder.finish_function()?;
    Ok(())
}
