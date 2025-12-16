use cx_bytecode_data::{
    types::{BCIntegerType, BCType, BCTypeKind},
    BCGlobalType, BCGlobalValue, BCInstructionKind, BCIntBinOp, BCPtrBinOp, BCValue,
};
use cx_typechecker_data::mir::{
    expression::{MIRInstruction, MIRUnOp, MIRValue},
    program::{MIRGlobalVarKind, MIRGlobalVariable},
    types::CXTypeKind,
};
use cx_util::{identifier::CXIdent, CXResult};

use crate::{
    builder::BCBuilder,
    mir_lowering::{
        binary_ops::{lower_binop, lower_call_params},
        coercion::lower_coercion,
        tagged_union::tagged_union_tag_addr,
    },
};

#[allow(dead_code)]
pub fn lower_instruction(
    builder: &mut BCBuilder,
    instruction: &MIRInstruction,
) -> CXResult<BCValue> {
    match instruction {
        MIRInstruction::Alias { result, value } => {
            let bc_value = lower_value(builder, value)?;
            builder.insert_symbol(result.clone(), bc_value);

            Ok(BCValue::NULL)
        }

        MIRInstruction::CopyRegionInto {
            destination,
            source,
            _type,
        } => {
            let bc_dest = lower_value(builder, destination)?;
            let bc_source = lower_value(builder, source)?;
            let bc_type = builder.convert_cx_type(_type);

            builder.add_instruction_translated(
                BCInstructionKind::Memcpy {
                    dest: bc_dest,
                    src: bc_source,
                    size: BCValue::IntImmediate {
                        val: bc_type.size() as i64,
                        _type: BCIntegerType::I64,
                    },
                    alignment: bc_type.alignment(),
                },
                BCType::default_pointer(),
                None,
            )?;

            Ok(BCValue::NULL)
        }

        MIRInstruction::CreateStackRegion { result, _type } => {
            let bc_type = builder.convert_cx_type(_type);

            builder.add_instruction_translated(
                BCInstructionKind::Allocate {
                    alignment: bc_type.alignment(),
                    _type: bc_type,
                },
                BCType::default_pointer(),
                Some(result.clone()),
            )
        }

        MIRInstruction::MemoryRead {
            result,
            source,
            _type,
        } => {
            let bc_source = lower_value(builder, source)?;
            let bc_type = builder.convert_cx_type(_type);

            builder.add_instruction_translated(
                BCInstructionKind::Load {
                    memory: bc_source,
                    _type: bc_type.clone(),
                },
                bc_type,
                Some(result.clone()),
            )
        }

        MIRInstruction::MemoryWrite { target, value } => {
            let bc_target = lower_value(builder, target)?;
            let bc_value = lower_value(builder, value)?;
            let bc_type = builder.get_value_type(&bc_value);

            builder.add_instruction_translated(
                BCInstructionKind::Store {
                    memory: bc_target,
                    value: bc_value,
                    _type: bc_type,
                },
                BCType::unit(),
                None,
            )
        }

        MIRInstruction::StructGet {
            result,
            source,
            field_index,
            field_offset,
            struct_type,
        } => {
            let bc_source = lower_value(builder, source)?;
            let bc_struct_type = builder.convert_cx_type(struct_type);

            builder.add_instruction_translated(
                BCInstructionKind::StructAccess {
                    struct_: bc_source,
                    struct_type: bc_struct_type,
                    field_index: *field_index,
                    field_offset: *field_offset,
                },
                BCType::default_pointer(),
                Some(result.clone()),
            )
        }

        MIRInstruction::TaggedUnionGet { result, source, .. } => {
            // For now, the content of a tagged union is stored plainly at the pointer location,
            // with the index appended at the end for alignment purposes. Thus, getting the value
            // is simply aliasing the pointer.
            let bc_source = lower_value(builder, source)?;

            builder.insert_symbol(result.clone(), bc_source.clone());

            Ok(bc_source)
        }

        MIRInstruction::TaggedUnionTag {
            result,
            source,
            sum_type,
        } => {
            let bc_source = lower_value(builder, source)?;
            let tagged_union = builder.convert_cx_type(sum_type);

            let tag_ptr = tagged_union_tag_addr(builder, bc_source.clone(), tagged_union)?;
            let tag_type = BCType::from(BCTypeKind::Integer(BCIntegerType::I8));

            builder.add_instruction_translated(
                BCInstructionKind::Load {
                    memory: tag_ptr,
                    _type: tag_type.clone(),
                },
                tag_type.clone(),
                Some(result.clone()),
            )
        }

        MIRInstruction::ConstructTaggedUnionInto {
            variant_index,
            memory,
            value,
            sum_type,
        } => {
            let value_type = value.get_type();

            let sum_as_bc_type = builder.convert_cx_type(sum_type);
            let value_as_bc_type = builder.convert_cx_type(&value_type);

            let bc_value = lower_value(builder, value)?;
            let bc_memory = lower_value(builder, memory)?;

            let tag_ptr = tagged_union_tag_addr(builder, bc_memory.clone(), sum_as_bc_type)?;
            builder.add_new_instruction(
                BCInstructionKind::Store {
                    memory: tag_ptr,
                    value: BCValue::IntImmediate {
                        val: *variant_index as i64,
                        _type: BCIntegerType::I8,
                    },
                    _type: value_as_bc_type.clone(),
                },
                BCType::unit(),
                false,
            )?;

            if value_type.is_memory_resident() {
                builder.add_new_instruction(
                    BCInstructionKind::Memcpy {
                        dest: bc_memory.clone(),
                        src: bc_value.clone(),
                        size: BCValue::IntImmediate {
                            val: sum_type.type_size() as i64,
                            _type: BCIntegerType::I64,
                        },
                        alignment: value_as_bc_type.alignment(),
                    },
                    BCType::default_pointer(),
                    false,
                )?;
            } else if !value_type.is_unit() {
                // For non-memory-resident types, we can store directly
                builder.add_instruction_translated(
                    BCInstructionKind::Store {
                        memory: bc_memory.clone(),
                        value: bc_value.clone(),
                        _type: value_as_bc_type.clone(),
                    },
                    BCType::unit(),
                    None,
                )?;
            }

            Ok(BCValue::NULL)
        }

        MIRInstruction::ArrayGet {
            result,
            source,
            index,
            element_type,
        } => {
            let bc_source = lower_value(builder, source)?;
            let bc_index = lower_value(builder, index)?;
            let bc_element_type = builder.convert_cx_type(element_type);

            let index_as_ptrdiff = builder.add_instruction_translated(
                BCInstructionKind::IntToPtrDiff {
                    value: bc_index,
                    ptr_inner: bc_element_type.clone(),
                },
                BCType::default_pointer(),
                None,
            )?;

            builder.add_instruction_translated(
                BCInstructionKind::PointerBinOp {
                    left: bc_source,
                    right: index_as_ptrdiff,
                    op: BCPtrBinOp::ADD,
                    ptr_type: bc_element_type,
                },
                BCType::default_pointer(),
                Some(result.clone()),
            )
        }

        MIRInstruction::CallFunction {
            result,
            function,
            arguments,
        } => {
            if let MIRValue::FunctionReference { prototype, .. } = function {
                let bc_prototype = builder.convert_cx_prototype(prototype);
                let return_type = builder.convert_cx_type(&prototype.return_type);
                let bc_arguments = lower_call_params(builder, &arguments, &bc_prototype)?;

                builder.add_instruction_translated(
                    BCInstructionKind::DirectCall {
                        args: bc_arguments,
                        method_sig: bc_prototype,
                    },
                    return_type,
                    result.as_ref().cloned(),
                )
            } else {
                let CXTypeKind::Function { prototype, .. } = function.get_type().kind else {
                    unreachable!("Function value is not of function type");
                };

                let bc_function = lower_value(builder, function)?;
                let bc_prototype = builder.convert_cx_prototype(prototype.as_ref());
                let return_type = builder.convert_cx_type(&prototype.return_type);
                let bc_arguments = lower_call_params(builder, &arguments, &bc_prototype)?;

                builder.add_instruction_translated(
                    BCInstructionKind::IndirectCall {
                        func_ptr: bc_function,
                        args: bc_arguments,
                        method_sig: bc_prototype,
                    },
                    return_type,
                    result.as_ref().cloned(),
                )
            }
        }

        MIRInstruction::LoopPreHeader {
            loop_id: _,
            condition_precheck,
            condition_block,
            body_block,
        } => {
            let jump_to = if *condition_precheck {
                condition_block.clone()
            } else {
                body_block.clone()
            };

            builder.add_instruction_translated(
                BCInstructionKind::Jump { target: jump_to },
                BCType::unit(),
                None,
            )?;

            Ok(BCValue::NULL)
        }

        MIRInstruction::LoopConditionBranch {
            loop_id: _,
            condition,
            body_block,
            exit_block,
        } => {
            let bc_condition = lower_value(builder, condition)?;

            builder.add_instruction_translated(
                BCInstructionKind::Branch {
                    condition: bc_condition,
                    true_block: body_block.clone(),
                    false_block: exit_block.clone(),
                },
                BCType::unit(),
                None,
            )?;

            Ok(BCValue::NULL)
        }

        MIRInstruction::LoopContinue {
            loop_id: _,
            condition_block,
        } => {
            builder.add_instruction_translated(
                BCInstructionKind::Jump {
                    target: condition_block.clone(),
                },
                BCType::unit(),
                None,
            )?;

            Ok(BCValue::NULL)
        }

        MIRInstruction::Branch {
            condition,
            true_block,
            false_block,
        } => {
            let bc_condition = lower_value(builder, condition)?;

            builder.add_instruction_translated(
                BCInstructionKind::Branch {
                    condition: bc_condition,
                    true_block: true_block.clone(),
                    false_block: false_block.clone(),
                },
                BCType::unit(),
                None,
            )?;

            Ok(BCValue::NULL)
        }

        MIRInstruction::Jump { target } => {
            builder.add_instruction_translated(
                BCInstructionKind::Jump {
                    target: target.clone(),
                },
                BCType::unit(),
                None,
            )?;

            Ok(BCValue::NULL)
        }

        MIRInstruction::JumpTable {
            condition,
            targets,
            default,
        } => {
            let bc_condition = lower_value(builder, condition)?;

            builder.add_instruction_translated(
                BCInstructionKind::JumpTable {
                    value: bc_condition,
                    targets: targets.clone(),
                    default: default.clone(),
                },
                BCType::unit(),
                None,
            )?;

            Ok(BCValue::NULL)
        }

        MIRInstruction::Return { value } => {
            let bc_value = value
                .as_ref()
                .map(|v| lower_value(builder, v))
                .transpose()?;

            let current_function = builder.current_prototype();

            if let Some(temp_buff) = &current_function.temp_buffer {
                let Some(return_value) = bc_value else {
                    unreachable!()
                };

                let return_buffer = BCValue::ParameterRef(0);
                builder.add_new_instruction(
                    BCInstructionKind::Memcpy {
                        dest: return_buffer.clone(),
                        src: return_value.clone(),
                        size: BCValue::IntImmediate {
                            val: temp_buff.size() as i64,
                            _type: BCIntegerType::I64,
                        },
                        alignment: current_function.return_type.alignment(),
                    },
                    BCType::unit(),
                    false,
                )?;

                builder.add_instruction_translated(
                    BCInstructionKind::Return {
                        value: Some(return_buffer),
                    },
                    BCType::unit(),
                    None,
                )?;
            } else {
                builder.add_instruction_translated(
                    BCInstructionKind::Return { value: bc_value },
                    BCType::unit(),
                    None,
                )?;
            }

            Ok(BCValue::NULL)
        }

        MIRInstruction::Phi {
            result,
            predecessors: incomings,
        } => {
            let result_type = incomings[0].0.get_type();
            let result_type = builder.convert_cx_type(&result_type);

            let predecessors = incomings
                .iter()
                .map(|(val, block)| {
                    let bc_val = lower_value(builder, val)?;
                    Ok((bc_val, block.clone()))
                })
                .collect::<CXResult<Vec<(BCValue, CXIdent)>>>()?;

            builder.add_instruction_translated(
                BCInstructionKind::Phi { predecessors },
                result_type,
                Some(result.clone()),
            )
        }

        MIRInstruction::BinOp {
            result,
            lhs,
            rhs,
            op,
        } => lower_binop(builder, result, op, lhs, rhs),

        MIRInstruction::UnOp {
            result,
            operand,
            op,
        } => match op {
            MIRUnOp::LNOT => {
                let bc_operand = lower_value(builder, operand)?;
                let result_type = builder.convert_cx_type(&operand.get_type());

                builder.add_instruction_translated(
                    BCInstructionKind::IntegerUnOp {
                        value: bc_operand,
                        op: cx_bytecode_data::BCIntUnOp::LNOT,
                    },
                    result_type,
                    Some(result.clone()),
                )
            }

            _ => todo!("Unary Operator: {:?}", op),
        },

        MIRInstruction::Coercion {
            result,
            operand,
            cast_type,
        } => lower_coercion(builder, result.clone(), operand, *cast_type),

        MIRInstruction::Assert { value, message } => {
            let global_string = builder.create_static_string(message.clone());
            let bc_condition = lower_value(builder, value)?;

            let prototype = builder
                .get_prototype("__compiler_assert")
                .expect("Compiler assert prototype not found");

            builder.add_instruction_translated(
                BCInstructionKind::DirectCall {
                    args: vec![bc_condition, global_string],
                    method_sig: prototype.clone(),
                },
                BCType::unit(),
                None,
            )
        }

        MIRInstruction::Assume { value } => {
            let bc_value = lower_value(builder, value)?;

            builder.add_instruction_translated(
                BCInstructionKind::CompilerAssumption {
                    condition: bc_value,
                },
                BCType::unit(),
                None,
            )
        }

        // Only relevant for verification, so no-op in bytecode
        MIRInstruction::Havoc { .. } => Ok(BCValue::NULL),
    }
}

pub(crate) fn lower_value(builder: &mut BCBuilder, value: &MIRValue) -> CXResult<BCValue> {
    match value {
        MIRValue::Register { register, _type: _ } => {
            builder.get_symbol(register).ok_or_else(|| {
                builder.dump_symbols();
                panic!(
                    "Register {:?} not found in symbol table for function {}",
                    register,
                    builder.current_function_name().unwrap()
                )
            })
        }

        MIRValue::FloatLiteral { value, _type } => {
            let bc_type = builder.convert_float_type(_type);

            Ok(BCValue::FloatImmediate {
                val: *value,
                _type: bc_type,
            })
        }

        MIRValue::IntLiteral {
            value,
            _type,
            signed: _,
        } => {
            let bc_type = builder.convert_integer_type(_type);

            Ok(BCValue::IntImmediate {
                val: *value,
                _type: bc_type,
            })
        }

        MIRValue::BoolLiteral { value } => Ok(BCValue::BoolImmediate(*value)),

        MIRValue::FunctionReference {
            prototype,
            implicit_variables: _,
        } => Ok(BCValue::FunctionRef(CXIdent::new(prototype.name.mangle()))),

        MIRValue::GlobalValue { name, _type } => {
            builder.get_global_symbol(name.as_str()).ok_or_else(|| {
                panic!(
                    "Global variable {:?} not found in global symbol table",
                    name
                )
            })
        }

        MIRValue::Parameter { name, _type: _ } => {
            let index = builder
                .current_prototype()
                .params
                .iter()
                .position(|param| {
                    param
                        .name
                        .as_ref()
                        .map(|p_name| p_name.as_str() == name.as_str())
                        .unwrap_or(false)
                })
                .expect("Parameter not found in function prototype");

            Ok(BCValue::ParameterRef(index as u32))
        }

        MIRValue::NULL => Ok(BCValue::NULL),
    }
}

pub(crate) fn lower_global_value(
    builder: &mut BCBuilder,
    global: &MIRGlobalVariable,
) -> CXResult<BCGlobalValue> {
    let bc_linkage = builder.convert_linkage(global.linkage);

    match &global.kind {
        MIRGlobalVarKind::StringLiteral { name, value } => Ok(BCGlobalValue {
            name: name.clone(),
            _type: BCGlobalType::StringLiteral(value.clone()),
            linkage: bc_linkage,
        }),

        MIRGlobalVarKind::Variable {
            name,
            _type,
            initializer,
        } => {
            let bc_type = builder.convert_cx_type(_type);
            let bc_initializer = initializer.clone();

            Ok(BCGlobalValue {
                name: name.clone(),
                _type: BCGlobalType::Variable {
                    _type: bc_type,
                    initial_value: bc_initializer,
                },
                linkage: bc_linkage,
            })
        }
    }
}
