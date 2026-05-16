use crate::inst_calling::{
    get_func_ref, get_method_return, prepare_function_sig, prepare_method_call, prepare_parameters,
};
use crate::routines::get_function;
use crate::value_type::get_cranelift_type;
use crate::{CodegenValue, FunctionState};
use cranelift::codegen::ir;
use cranelift::codegen::ir::stackslot::StackSize;
use cranelift::codegen::ir::InstructionData;
use cranelift::frontend::Switch;
use cranelift::prelude::{InstBuilder, MemFlags, StackSlotData, StackSlotKind};
use cranelift_module::Module;
use cx_lmir::types::{LMIRFloatType, LMIRTypeKind};
use cx_lmir::{
    LMIRCoercionType, LMIRFloatBinOp, LMIRFloatUnOp, LMIRInstruction, LMIRInstructionKind,
    LMIRIntBinOp, LMIRIntUnOp, LMIRPtrBinOp,
};
use cx_util::CXResult;
use std::ops::IndexMut;

fn load_return_slots(
    context: &mut FunctionState,
    target: cranelift::prelude::Value,
) -> CXResult<Vec<cranelift::prelude::Value>> {
    let cx_lmir::LMIRReturnABI::Direct { slots } = &context.signature.return_abi else {
        return Ok(vec![target]);
    };

    let mut values = Vec::new();
    for slot in slots.clone() {
        values.push(context.builder.ins().load(
            get_cranelift_type(&slot._type)?,
            MemFlags::new(),
            target,
            slot.offset as i32,
        ));
    }
    Ok(values)
}

pub(crate) fn codegen_instruction(
    context: &mut FunctionState,
    instruction: &LMIRInstruction,
) -> CXResult<CodegenValue> {
    Ok(match &instruction.kind {
        LMIRInstructionKind::Alias { value } => context.get_value(value)?,

        LMIRInstructionKind::Allocate { _type, alignment } => {
            let slot = context.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                StackSize::from(_type.size() as u16),
                *alignment,
            ));

            CodegenValue::Value(
                context
                    .builder
                    .ins()
                    .stack_addr(context.pointer_type, slot, 0),
            )
        }

        LMIRInstructionKind::DirectCall {
            func,
            args,
            method_sig,
        } => {
            let id = get_function(context, func.as_str(), method_sig)?;
            let params = prepare_parameters(context, args)?;
            let fn_ref = get_func_ref(context, id, method_sig, params.as_slice())?;
            let inst = context.builder.ins().call(fn_ref, params.as_slice());

            get_method_return(context, method_sig, inst)
        }

        LMIRInstructionKind::IndirectCall {
            func_ptr,
            args,
            method_sig,
        } => {
            let (val, params) = prepare_method_call(context, func_ptr, args)?;

            let mut sig = prepare_function_sig(context.object_module, method_sig)?;

            for i in method_sig.expanded_param_count()..params.len() {
                let arg = params[i];
                let arg_type = context.builder.func.dfg.value_type(arg);

                sig.params.push(ir::AbiParam::new(arg_type));
            }

            let sig_ref = context.builder.import_signature(sig);

            let inst =
                context
                    .builder
                    .ins()
                    .call_indirect(sig_ref, val.as_value(), params.as_slice());

            get_method_return(context, method_sig, inst)
        }

        LMIRInstructionKind::Return { value } => {
            match value {
                Some(value) => {
                    let return_value = context.get_value(value)?;
                    match return_value {
                        CodegenValue::Value(value)
                            if context.signature.return_type.is_memory_resident()
                                && matches!(
                                    context.signature.return_abi,
                                    cx_lmir::LMIRReturnABI::Direct { .. }
                                ) =>
                        {
                            let values = load_return_slots(context, value)?;
                            context.builder.ins().return_(values.as_slice())
                        }
                        CodegenValue::Value(value) => context.builder.ins().return_(&[value]),
                        CodegenValue::Aggregate(values) => {
                            context.builder.ins().return_(values.as_slice())
                        }
                        CodegenValue::AggregateSlots(values) => {
                            let values = values
                                .into_iter()
                                .map(|(_, value)| value)
                                .collect::<Vec<_>>();
                            context.builder.ins().return_(values.as_slice())
                        }
                        CodegenValue::NULL => context.builder.ins().return_(&[]),
                    };
                }
                None => {
                    context.builder.ins().return_(&[]);
                }
            };

            CodegenValue::NULL
        }

        LMIRInstructionKind::Load { memory, _type } => {
            let target = context.get_value(memory)?.as_value();

            CodegenValue::Value(context.builder.ins().load(
                get_cranelift_type(_type)?,
                MemFlags::new(),
                target,
                0,
            ))
        }

        LMIRInstructionKind::GetFunctionAddr { func } => {
            let Some(id) = context.function_ids.get(func.as_str()).cloned() else {
                panic!("INTERNAL ERROR: Function not found in codegen for GetFunctionAddr: {func}");
            };

            let func_ref = context
                .object_module
                .declare_func_in_func(id, context.builder.func);

            let pointer = context.pointer_type;
            CodegenValue::Value(context.builder.ins().func_addr(pointer, func_ref))
        }

        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::PtrToInt,
        } => {
            let bytes = instruction.value_type.size();
            let val = context.get_value(value)?;

            if bytes < 8 {
                CodegenValue::Value(
                    context.builder.ins().ireduce(
                        ir::Type::int(bytes as u16 * 8)
                            .unwrap_or_else(|| panic!("Unsupported integer size: {}", bytes * 8)),
                        val.as_value(),
                    ),
                )
            } else {
                val
            }
        }

        LMIRInstructionKind::Coercion {
            coercion_type:
                LMIRCoercionType::IntToPtr {
                    from: _type,
                    sextend,
                },
            value,
        } => {
            let val = context.get_value(value)?;

            if _type.bytes() < 8 {
                let _ty = get_cranelift_type(&instruction.value_type)?;

                if *sextend {
                    CodegenValue::Value(context.builder.ins().uextend(_ty, val.as_value()))
                } else {
                    CodegenValue::Value(context.builder.ins().sextend(_ty, val.as_value()))
                }
            } else {
                val
            }
        }

        LMIRInstructionKind::PointerBinOp {
            op,
            left,
            right,
            type_padded_size,
            ..
        } => {
            let left = context.get_value(left)?.as_value();
            let right = context.get_value(right)?.as_value();
            let _type = &instruction.value_type;

            let inst =
                match op {
                    LMIRPtrBinOp::ADD => {
                        let right_scaled = context
                            .builder
                            .ins()
                            .imul_imm(right, *type_padded_size as i64); // assuming type_padded_size is 1 for simplicity

                        context.builder.ins().iadd(left, right_scaled)
                    }

                    LMIRPtrBinOp::SUB => {
                        let right_scaled = context
                            .builder
                            .ins()
                            .imul_imm(right, *type_padded_size as i64);

                        context.builder.ins().isub(left, right_scaled)
                    }

                    LMIRPtrBinOp::EQ => {
                        context
                            .builder
                            .ins()
                            .icmp(ir::condcodes::IntCC::Equal, left, right)
                    }
                    LMIRPtrBinOp::NE => {
                        context
                            .builder
                            .ins()
                            .icmp(ir::condcodes::IntCC::NotEqual, left, right)
                    }

                    LMIRPtrBinOp::LT => context.builder.ins().icmp(
                        ir::condcodes::IntCC::SignedLessThan,
                        left,
                        right,
                    ),
                    LMIRPtrBinOp::GT => context.builder.ins().icmp(
                        ir::condcodes::IntCC::SignedGreaterThan,
                        left,
                        right,
                    ),
                    LMIRPtrBinOp::LE => context.builder.ins().icmp(
                        ir::condcodes::IntCC::SignedLessThanOrEqual,
                        left,
                        right,
                    ),
                    LMIRPtrBinOp::GE => context.builder.ins().icmp(
                        ir::condcodes::IntCC::SignedGreaterThanOrEqual,
                        left,
                        right,
                    ),
                };

            CodegenValue::Value(inst)
        }

        LMIRInstructionKind::IntegerBinOp { op, left, right } => {
            let left = context.get_value(left)?.as_value();
            let right = context.get_value(right)?.as_value();

            let inst = match op {
                LMIRIntBinOp::ADD => context.builder.ins().iadd(left, right),
                LMIRIntBinOp::SUB => context.builder.ins().isub(left, right),
                LMIRIntBinOp::IMUL | LMIRIntBinOp::MUL => context.builder.ins().imul(left, right),
                LMIRIntBinOp::IDIV => context.builder.ins().sdiv(left, right),
                LMIRIntBinOp::IREM => context.builder.ins().srem(left, right),

                LMIRIntBinOp::UDIV => context.builder.ins().udiv(left, right),
                LMIRIntBinOp::UREM => context.builder.ins().urem(left, right),

                LMIRIntBinOp::SHL => context.builder.ins().ishl(left, right),
                LMIRIntBinOp::ASHR => context.builder.ins().sshr(left, right),
                LMIRIntBinOp::LSHR => context.builder.ins().ushr(left, right),

                LMIRIntBinOp::BAND => context.builder.ins().band(left, right),
                LMIRIntBinOp::BOR => context.builder.ins().bor(left, right),
                LMIRIntBinOp::BXOR => context.builder.ins().bxor(left, right),

                LMIRIntBinOp::LAND => {
                    let left =
                        context
                            .builder
                            .ins()
                            .icmp_imm(ir::condcodes::IntCC::NotEqual, left, 0);
                    let right =
                        context
                            .builder
                            .ins()
                            .icmp_imm(ir::condcodes::IntCC::NotEqual, right, 0);

                    context.builder.ins().band(left, right)
                }

                LMIRIntBinOp::LOR => {
                    let left = context
                        .builder
                        .ins()
                        .icmp_imm(ir::condcodes::IntCC::Equal, left, 0);
                    let right =
                        context
                            .builder
                            .ins()
                            .icmp_imm(ir::condcodes::IntCC::Equal, right, 0);

                    context.builder.ins().bor(left, right)
                }

                LMIRIntBinOp::EQ => {
                    context
                        .builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::Equal, left, right)
                }
                LMIRIntBinOp::NE => {
                    context
                        .builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::NotEqual, left, right)
                }

                LMIRIntBinOp::ILT => {
                    context
                        .builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::SignedLessThan, left, right)
                }
                LMIRIntBinOp::IGT => {
                    context
                        .builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::SignedGreaterThan, left, right)
                }
                LMIRIntBinOp::ILE => context.builder.ins().icmp(
                    ir::condcodes::IntCC::SignedLessThanOrEqual,
                    left,
                    right,
                ),
                LMIRIntBinOp::IGE => context.builder.ins().icmp(
                    ir::condcodes::IntCC::SignedGreaterThanOrEqual,
                    left,
                    right,
                ),

                LMIRIntBinOp::ULT => {
                    context
                        .builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::UnsignedLessThan, left, right)
                }
                LMIRIntBinOp::UGT => context.builder.ins().icmp(
                    ir::condcodes::IntCC::UnsignedGreaterThan,
                    left,
                    right,
                ),
                LMIRIntBinOp::ULE => context.builder.ins().icmp(
                    ir::condcodes::IntCC::UnsignedLessThanOrEqual,
                    left,
                    right,
                ),
                LMIRIntBinOp::UGE => context.builder.ins().icmp(
                    ir::condcodes::IntCC::UnsignedGreaterThanOrEqual,
                    left,
                    right,
                ),
            };

            CodegenValue::Value(inst)
        }

        LMIRInstructionKind::IntegerUnOp { op, value } => {
            let val = context.get_value(value)?;

            let inst = match op {
                LMIRIntUnOp::NEG => context.builder.ins().ineg(val.as_value()),
                LMIRIntUnOp::BNOT => context.builder.ins().bnot(val.as_value()),
                LMIRIntUnOp::LNOT => {
                    context
                        .builder
                        .ins()
                        .icmp_imm(ir::condcodes::IntCC::Equal, val.as_value(), 0)
                }
            };

            CodegenValue::Value(inst)
        }

        LMIRInstructionKind::FloatUnOp { value, op } => {
            let val = context.get_value(value)?;
            let _type = &instruction.value_type;

            match op {
                LMIRFloatUnOp::NEG => {
                    CodegenValue::Value(context.builder.ins().fneg(val.as_value()))
                }
            }
        }

        LMIRInstructionKind::FloatBinOp { left, right, op } => {
            let left = context.get_value(left)?.as_value();
            let right = context.get_value(right)?.as_value();

            CodegenValue::Value(match op {
                LMIRFloatBinOp::ADD => context.builder.ins().fadd(left, right),
                LMIRFloatBinOp::SUB => context.builder.ins().fsub(left, right),
                LMIRFloatBinOp::FMUL => context.builder.ins().fmul(left, right),
                LMIRFloatBinOp::FDIV => context.builder.ins().fdiv(left, right),

                LMIRFloatBinOp::EQ => {
                    context
                        .builder
                        .ins()
                        .fcmp(ir::condcodes::FloatCC::Equal, left, right)
                }
                LMIRFloatBinOp::NEQ => {
                    context
                        .builder
                        .ins()
                        .fcmp(ir::condcodes::FloatCC::NotEqual, left, right)
                }
                LMIRFloatBinOp::FLT => {
                    context
                        .builder
                        .ins()
                        .fcmp(ir::condcodes::FloatCC::LessThan, left, right)
                }
                LMIRFloatBinOp::FLE => {
                    context
                        .builder
                        .ins()
                        .fcmp(ir::condcodes::FloatCC::LessThanOrEqual, left, right)
                }
                LMIRFloatBinOp::FGT => {
                    context
                        .builder
                        .ins()
                        .fcmp(ir::condcodes::FloatCC::GreaterThan, left, right)
                }
                LMIRFloatBinOp::FGE => context.builder.ins().fcmp(
                    ir::condcodes::FloatCC::GreaterThanOrEqual,
                    left,
                    right,
                ),
            })
        }

        LMIRInstructionKind::Branch {
            condition,
            true_block,
            false_block,
        } => {
            let condition = context.get_value(condition)?.as_value();
            let true_block = context.get_block(true_block);
            let false_block = context.get_block(false_block);

            context
                .builder
                .ins()
                .brif(condition, true_block, &[], false_block, &[]);

            CodegenValue::NULL
        }

        LMIRInstructionKind::Jump { target } => {
            let target = context.get_block(target);

            context.builder.ins().jump(target, &[]);

            CodegenValue::NULL
        }

        LMIRInstructionKind::StructAccess {
            struct_,
            field_offset,
            ..
        } => {
            let ptr = context.get_value(struct_)?.clone();
            let _type = &instruction.value_type;

            CodegenValue::Value(
                context
                    .builder
                    .ins()
                    .iadd_imm(ptr.as_value(), *field_offset as i64),
            )
        }

        LMIRInstructionKind::Store {
            memory,
            value,
            _type,
        } => {
            let target = context.get_value(memory)?.as_value();
            let value = context.get_value(value)?;

            match value {
                CodegenValue::AggregateSlots(values) => {
                    for (slot, value) in values {
                        context.builder.ins().store(
                            MemFlags::new(),
                            value,
                            target,
                            slot.offset as i32,
                        );
                    }
                }
                value => {
                    context
                        .builder
                        .ins()
                        .store(MemFlags::new(), value.as_value(), target, 0);
                }
            }

            CodegenValue::NULL
        }

        LMIRInstructionKind::Memcpy {
            dest,
            src,
            size,
            alignment: _,
        } => {
            let dest = context.get_value(dest)?.as_value();
            let src = context.get_value(src)?.as_value();
            let size = context.get_value(size)?.as_value();

            context
                .builder
                .call_memcpy(*context.target_frontend_config, dest, src, size);

            CodegenValue::NULL
        }

        LMIRInstructionKind::ZeroMemory { memory, _type } => {
            let target = context.get_value(memory)?.as_value();
            let size_literal = context
                .builder
                .ins()
                .iconst(ir::Type::int(64).unwrap(), _type.size() as i64);

            let zero = context.builder.ins().iconst(ir::Type::int(8).unwrap(), 0);

            context.builder.call_memset(
                *context.target_frontend_config,
                target,
                zero,
                size_literal,
            );

            CodegenValue::NULL
        }

        LMIRInstructionKind::Phi { predecessors } => {
            let current_block = context.builder.current_block().unwrap();
            let current_block_len = context
                .builder
                .func
                .layout
                .block_insts(current_block)
                .count();

            context
                .builder
                .append_block_param(current_block, get_cranelift_type(&instruction.value_type)?);

            for (from_value, from_block) in predecessors {
                let block = context.get_block(from_block);

                // get last instruction in the block
                let last_inst = context
                    .builder
                    .func
                    .layout
                    .block_insts(block)
                    .next_back()
                    .unwrap();

                // code made with only the worst of intentions
                context.builder.func.layout.remove_inst(last_inst);

                let value = context.get_value(from_value)?.as_value();

                while context
                    .builder
                    .func
                    .layout
                    .block_insts(current_block)
                    .count()
                    > current_block_len
                {
                    let inst = context
                        .builder
                        .func
                        .layout
                        .block_insts(current_block)
                        .next_back()
                        .unwrap();
                    context.builder.func.layout.remove_inst(inst);
                    context.builder.func.layout.append_inst(inst, block);
                }

                context.builder.func.layout.append_inst(last_inst, block);

                unsafe {
                    let value_pool: *mut _ = &mut context.builder.func.dfg.value_lists;

                    match context.builder.func.dfg.insts.index_mut(last_inst) {
                        InstructionData::Jump { destination, .. } => {
                            destination.append_argument(value, &mut *value_pool);
                        }
                        InstructionData::Brif { blocks, .. } => {
                            if blocks.get_mut(0).unwrap().block(&*value_pool) == current_block {
                                blocks
                                    .get_mut(0)
                                    .unwrap()
                                    .append_argument(value, &mut *value_pool);
                            }
                            if blocks.get_mut(1).unwrap().block(&*value_pool) == current_block {
                                blocks
                                    .get_mut(1)
                                    .unwrap()
                                    .append_argument(value, &mut *value_pool);
                            }
                        }
                        _ => {
                            panic!("Invalid instruction type for Phi: {last_inst:?}");
                        }
                    }
                }
            }

            let val = context
                .builder
                .block_params(current_block)
                .last()
                .cloned()
                .expect("No block parameter found for Phi instruction");

            CodegenValue::Value(val)
        }

        LMIRInstructionKind::Coercion {
            coercion_type: LMIRCoercionType::ZExtend,
            value,
        } => {
            let val = context.get_value(value)?.as_value();
            let _type = &instruction.value_type;
            let cranelift_type = get_cranelift_type(_type)?;

            let val_type = context.builder.func.dfg.value_type(val);

            if val_type == cranelift_type {
                CodegenValue::Value(val)
            } else {
                CodegenValue::Value(context.builder.ins().uextend(cranelift_type, val))
            }
        }

        LMIRInstructionKind::Coercion {
            coercion_type: LMIRCoercionType::SExtend,
            value,
        } => {
            let val = context.get_value(value)?.as_value();

            let _type = &instruction.value_type;
            let cranelift_type = get_cranelift_type(_type)?;

            let val_type = context.builder.func.dfg.value_type(val);

            if val_type == cranelift_type {
                CodegenValue::Value(val)
            } else {
                CodegenValue::Value(context.builder.ins().sextend(cranelift_type, val))
            }
        }

        LMIRInstructionKind::Coercion {
            coercion_type: LMIRCoercionType::Trunc,
            value,
        } => {
            let val = context.get_value(value)?;
            let _type = &instruction.value_type;

            let value = val.as_value();
            let cranelift_type = get_cranelift_type(_type)?;
            let value_type = context.builder.func.dfg.value_type(value);

            if value_type == cranelift_type {
                CodegenValue::Value(value)
            } else {
                CodegenValue::Value(
                    context
                        .builder
                        .ins()
                        .ireduce(cranelift_type, val.as_value()),
                )
            }
        }

        LMIRInstructionKind::Coercion {
            coercion_type: LMIRCoercionType::BitCast,
            value,
        } => context.get_value(value)?,

        LMIRInstructionKind::Coercion {
            coercion_type: LMIRCoercionType::FloatCast { from: _ },
            value,
        } => {
            let val = context.get_value(value)?;
            let to_type = &instruction.value_type;
            let cranelift_type = get_cranelift_type(to_type)?;

            match &to_type.kind {
                LMIRTypeKind::Float(LMIRFloatType::F32) => CodegenValue::Value(
                    context
                        .builder
                        .ins()
                        .fdemote(cranelift_type, val.as_value()),
                ),
                LMIRTypeKind::Float(LMIRFloatType::F64) => CodegenValue::Value(
                    context
                        .builder
                        .ins()
                        .fpromote(cranelift_type, val.as_value()),
                ),
                _ => unreachable!("Invalid type for float cast"),
            }
        }

        LMIRInstructionKind::Coercion {
            coercion_type:
                LMIRCoercionType::FloatToInt {
                    from,
                    sextend: signed,
                },
            value,
        } => {
            let val = context.get_value(value)?.as_value();
            let _type = &instruction.value_type;

            let to_cl_type = get_cranelift_type(_type)?;

            let LMIRTypeKind::Integer(itype) = &_type.kind else {
                panic!("Invalid type for float to int conversion")
            };

            let ival = if *signed {
                context.builder.ins().fcvt_to_sint(to_cl_type, val)
            } else {
                context.builder.ins().fcvt_to_uint(to_cl_type, val)
            };

            let float_bytes = from.bytes();
            let int_bytes = itype.bytes();

            let val = if float_bytes > int_bytes {
                context.builder.ins().ireduce(to_cl_type, ival)
            } else if float_bytes < int_bytes && *signed {
                context.builder.ins().sextend(to_cl_type, ival)
            } else if float_bytes < int_bytes {
                context.builder.ins().uextend(to_cl_type, ival)
            } else {
                ival
            };

            CodegenValue::Value(val)
        }

        LMIRInstructionKind::Coercion {
            coercion_type: LMIRCoercionType::IntToFloat { from: _, sextend },
            value,
        } => {
            let val = context.get_value(value)?.as_value();
            let _type = &instruction.value_type;

            let to_cl_type = get_cranelift_type(_type)?;

            CodegenValue::Value(if *sextend {
                context.builder.ins().fcvt_from_sint(to_cl_type, val)
            } else {
                context.builder.ins().fcvt_from_uint(to_cl_type, val)
            })
        }

        LMIRInstructionKind::JumpTable {
            value,
            targets,
            default,
        } => {
            let mut switch = Switch::new();

            for (value, block_id) in targets {
                switch.set_entry(*value as u128, context.get_block(block_id));
            }

            let default_block = context.get_block(default);
            let value = context.get_value(value)?;

            switch.emit(&mut context.builder, value.as_value(), default_block);

            CodegenValue::NULL
        }

        LMIRInstructionKind::CompilerAssumption { .. } => CodegenValue::NULL,
    })
}
