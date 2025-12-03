use cx_bytecode_data::{
    types::{BCIntegerType, BCType, BCTypeKind},
    BCCoercionType, BCGlobalValue, BCInstructionKind, BCIntBinOp, BCPtrBinOp, BCValue,
};
use cx_typechecker_data::mir::{
    expression::{MIRBinOp, MIRCoercion, MIRInstruction, MIRUnOp, MIRValue},
    types::CXTypeKind,
};
use cx_util::{identifier::CXIdent, CXResult};

use crate::{builder::BCBuilder, mir_lowering::tagged_union::tagged_union_tag_addr};

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

        MIRInstruction::CopyStackRegionInto {
            result,
            source,
            _type,
        } => {
            let bc_source = builder.get_symbol(source).unwrap();

            let bc_type = builder.convert_cx_type(_type);
            let new_region = builder.add_instruction(
                BCInstructionKind::Allocate {
                    alignment: bc_type.alignment(),
                    _type: bc_type.clone(),
                },
                BCType::default_pointer(),
                None,
            )?;

            builder.add_instruction(
                BCInstructionKind::Store {
                    memory: new_region.clone(),
                    value: bc_source.clone(),
                    _type: bc_type,
                },
                BCType::default_pointer(),
                None,
            )?;

            builder.insert_symbol(result.clone(), new_region);

            Ok(bc_source)
        }

        MIRInstruction::CreateEmptyStackRegion { result, _type } => {
            let bc_type = builder.convert_cx_type(_type);

            builder.add_instruction(
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

            builder.add_instruction(
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

            builder.add_instruction(
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

            builder.add_instruction(
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

        MIRInstruction::TaggedUnionIs {
            result,
            source,
            tag_id,
        } => {
            let bc_source = lower_value(builder, source)?;
            let tagged_union = builder.get_value_type(&bc_source);

            let tag_ptr = tagged_union_tag_addr(builder, bc_source.clone(), tagged_union)?;
            let tag_type = BCType::from(BCTypeKind::Integer(BCIntegerType::I8));

            let actual_tag_id = builder.add_instruction(
                BCInstructionKind::Load {
                    memory: tag_ptr,
                    _type: tag_type.clone(),
                },
                tag_type.clone(),
                None,
            )?;

            builder.add_instruction(
                BCInstructionKind::IntegerBinOp {
                    left: actual_tag_id,
                    right: BCValue::IntImmediate {
                        val: *tag_id as i64,
                        _type: BCIntegerType::I8,
                    },
                    op: BCIntBinOp::EQ,
                },
                BCTypeKind::Bool.into(),
                Some(result.clone()),
            )
        }

        MIRInstruction::ConstructTaggedUnionInto {
            variant_index,
            memory,
            value,
            sum_type,
        } => {
            let sum_as_bc_type = builder.convert_cx_type(sum_type);
            let value_as_bc_type = builder.convert_cx_type(&value.get_type());

            let bc_value = lower_value(builder, value)?;
            let bc_memory = lower_value(builder, memory)?;

            let tag_ptr = tagged_union_tag_addr(builder, bc_value.clone(), sum_as_bc_type)?;

            builder.add_instruction(
                BCInstructionKind::Store {
                    memory: bc_memory,
                    value: bc_value,
                    _type: value_as_bc_type,
                },
                BCType::unit(),
                None,
            )?;

            builder.add_instruction(
                BCInstructionKind::Store {
                    memory: tag_ptr,
                    value: BCValue::IntImmediate {
                        val: *variant_index as i64,
                        _type: BCIntegerType::I8,
                    },
                    _type: BCType::from(BCTypeKind::Integer(BCIntegerType::I8)),
                },
                BCType::unit(),
                None,
            )?;

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

            let index_as_ptrdiff = builder.add_instruction(
                BCInstructionKind::IntToPtrDiff {
                    value: bc_index,
                    ptr_inner: bc_element_type.clone(),
                },
                BCType::default_pointer(),
                None,
            )?;

            builder.add_instruction(
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
            let bc_arguments = arguments
                .iter()
                .map(|arg| lower_value(builder, arg))
                .collect::<CXResult<Vec<BCValue>>>()?;

            if let MIRValue::FunctionReference { prototype, .. } = function {
                let bc_prototype = builder.convert_cx_prototype(prototype);

                builder.add_instruction(
                    BCInstructionKind::DirectCall {
                        args: bc_arguments,
                        method_sig: bc_prototype,
                    },
                    BCType::unit(), // Placeholder, should be the actual return type
                    result.as_ref().cloned(),
                )
            } else {
                let CXTypeKind::Function { prototype, .. } = function.get_type().kind else {
                    unreachable!("Function value is not of function type");
                };

                let bc_function = lower_value(builder, function)?;
                let bc_prototype = builder.convert_cx_prototype(prototype.as_ref());

                builder.add_instruction(
                    BCInstructionKind::IndirectCall {
                        func_ptr: bc_function,
                        args: bc_arguments,
                        method_sig: bc_prototype,
                    },
                    BCType::unit(), // Placeholder, should be the actual return type
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

            builder.add_instruction(
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

            builder.add_instruction(
                BCInstructionKind::Branch {
                    condition: bc_condition,
                    true_block: body_block.clone(),
                    false_block: exit_block.clone(),
                },
                BCType::unit(),
                None,
            )?;

            Ok(BCValue::NULL)
        },
        
        MIRInstruction::LoopContinue {
            loop_id: _,
            condition_block,
        } => {
            builder.add_instruction(
                BCInstructionKind::Jump {
                    target: condition_block.clone(),
                },
                BCType::unit(),
                None,
            )?;

            Ok(BCValue::NULL)
        },

        MIRInstruction::Branch {
            condition,
            true_block,
            false_block,
        } => {
            let bc_condition = lower_value(builder, condition)?;

            builder.add_instruction(
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
            builder.add_instruction(
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

            builder.add_instruction(
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

            builder.add_instruction(
                BCInstructionKind::Return { value: bc_value },
                BCType::unit(),
                None,
            )?;

            Ok(BCValue::NULL)
        }

        MIRInstruction::BinOp {
            result,
            lhs,
            rhs,
            op,
        } => match op {
            _ => todo!("Operator: {:?}", op),
        },

        MIRInstruction::UnOp {
            result,
            operand,
            op,
        } => match op {
            MIRUnOp::LNOT => {
                let bc_operand = lower_value(builder, operand)?;
                let result_type = builder.convert_cx_type(&operand.get_type());

                builder.add_instruction(
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
        } => {
            let bc_operand = lower_value(builder, operand)?;

            match cast_type {
                MIRCoercion::Integral { sextend, to_type } => {
                    let bc_to_type = builder.convert_cx_type(
                        &CXTypeKind::Integer {
                            signed: false,
                            _type: *to_type,
                        }
                        .into(),
                    );

                    if to_type.bytes() < operand.get_type().type_size() {
                        builder.add_instruction(
                            BCInstructionKind::Coerce {
                                value: bc_operand,
                                coercion_type: BCCoercionType::Trunc,
                            },
                            bc_to_type,
                            Some(result.clone()),
                        )
                    } else {
                        builder.add_instruction(
                            BCInstructionKind::Coerce {
                                value: bc_operand,
                                coercion_type: if *sextend {
                                    BCCoercionType::SExtend
                                } else {
                                    BCCoercionType::ZExtend
                                },
                            },
                            bc_to_type,
                            Some(result.clone()),
                        )
                    }
                }

                _ => todo!("Coercion: {cast_type:?}"),
            }
        }

        MIRInstruction::Assert { value, message } => {
            let global_string = builder.create_static_string(message.clone());

            let bc_condition = lower_value(builder, value)?;

            builder.add_instruction(
                BCInstructionKind::CompilerAssertion {
                    condition: bc_condition,
                    message: global_string,
                },
                BCType::unit(),
                None,
            )
        }

        MIRInstruction::Assume { value } => {
            let bc_value = lower_value(builder, value)?;

            builder.add_instruction(
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

        MIRValue::FunctionReference {
            prototype,
            implicit_variables: _,
        } => Ok(BCValue::FunctionRef(CXIdent::new(prototype.name.mangle()))),

        MIRValue::GlobalValue { name, _type } => {
            // let global = lower_global_variable(builder, name, &builder.convert_cx_type(_type))?;

            Ok(BCValue::NULL)
        }

        MIRValue::Parameter { index, _type: _ } => Ok(BCValue::ParameterRef(*index as u32)),

        MIRValue::NULL => Ok(BCValue::NULL),
    }
}

fn lower_global_variable(
    builder: &mut BCBuilder,
    name: &CXIdent,
    _type: &BCType,
) -> CXResult<BCGlobalValue> {
    todo!()
}
