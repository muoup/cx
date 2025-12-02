use crate::inst_calling::{
    get_func_ref, get_method_return, prepare_method_call, prepare_parameters,
};
use crate::routines::get_function;
use crate::value_type::{get_cranelift_abi_type, get_cranelift_type};
use crate::{CodegenValue, FunctionState};
use cranelift::codegen::ir;
use cranelift::codegen::ir::stackslot::StackSize;
use cranelift::codegen::ir::InstructionData;
use cranelift::frontend::Switch;
use cranelift::prelude::{Imm64, InstBuilder, MemFlags, StackSlotData, StackSlotKind};
use cranelift_module::Module;
use cx_bytecode_data::types::{BCType, BCTypeKind, MIRTypeSize};
use cx_bytecode_data::{
    BCFloatBinOp, BCFloatUnOp, BCIntUnOp, BCPtrBinOp, BCInstruction, LinkageType,
    MIRFunctionPrototype, BCIntBinOp, MIRParameter, BCInstructionKind,
};
use std::ops::IndexMut;

pub(crate) fn codegen_instruction(
    context: &mut FunctionState,
    instruction: &BCInstruction,
) -> Option<CodegenValue> {
    match &instruction.kind {
        BCInstructionKind::Alias { value } => context.get_value(value),

        BCInstructionKind::Allocate { _type, alignment } => {
            let slot = match _type.size() {
                MIRTypeSize::Fixed(size) => {
                    context.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        StackSize::from(size as u16),
                        *alignment,
                    ))
                }
                MIRTypeSize::Variable(_) => panic!(
                    "Cranelift does not support variable sized stack slots, use LLVM instead"
                ),
            };

            Some(CodegenValue::Value(context.builder.ins().stack_addr(
                context.pointer_type,
                slot,
                0,
            )))
        }

        BCInstructionKind::DirectCall {
            args, method_sig, ..
        } => {
            let Some(id) = get_function(context, method_sig) else {
                panic!("Failed to call function {}", &method_sig.name);
            };

            let Some(params) = prepare_parameters(context, args) else {
                panic!(
                    "Failed to prepare parameters for DirectCall: {}",
                    method_sig.name
                );
            };

            let Some(fn_ref) = get_func_ref(context, id, method_sig, params.as_slice()) else {
                panic!(
                    "Failed to get function reference for DirectCall: {}",
                    method_sig.name
                );
            };

            let inst = context.builder.ins().call(fn_ref, params.as_slice());

            get_method_return(context, inst)
        }

        BCInstructionKind::IndirectCall {
            func_ptr,
            args,
            method_sig,
        } => {
            let (val, params) = prepare_method_call(context, func_ptr, args)?;

            let mut sig = context.object_module.make_signature();
            let return_type = &method_sig.return_type;

            sig.returns = if return_type.is_void() {
                vec![]
            } else {
                vec![get_cranelift_abi_type(return_type)]
            };
            sig.params = method_sig
                .params
                .iter()
                .map(|arg| get_cranelift_abi_type(&arg._type))
                .collect();

            for i in method_sig.params.len()..params.len() {
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

            get_method_return(context, inst)
        }

        BCInstructionKind::Return { value } => {
            match value {
                Some(value) => {
                    let return_value = context.get_value(value).unwrap();
                    context.builder.ins().return_(&[return_value.as_value()]);
                }
                None => {
                    context.builder.ins().return_(&[]);
                }
            };

            None
        }

        BCInstructionKind::GetFunctionAddr { func } => {
            let Some(id) = context.function_ids.get(func.as_str()).cloned() else {
                panic!("INTERNAL ERROR: Function not found in codegen for GetFunctionAddr: {func}");
            };

            let func_ref = context
                .object_module
                .declare_func_in_func(id, context.builder.func);

            let pointer = context.pointer_type;
            Some(CodegenValue::Value(
                context.builder.ins().func_addr(pointer, func_ref),
            ))
        }

        BCInstructionKind::PtrToInt { value } => {
            let bytes = match instruction.value_type.kind {
                BCTypeKind::Signed { bytes, .. } => bytes,
                BCTypeKind::Integer { bytes, .. } => bytes,
                _ => panic!("Invalid type for pointer to integer conversion"),
            };

            let val = context.get_value(value).unwrap();

            if bytes < 8 {
                return Some(CodegenValue::Value(
                    context.builder.ins().ireduce(
                        ir::Type::int(bytes as u16 * 8)
                            .unwrap_or_else(|| panic!("Unsupported integer size: {}", bytes * 8)),
                        val.as_value(),
                    ),
                ));
            };

            Some(val)
        }

        BCInstructionKind::IntToPtrDiff { value, ptr_type } => {
            let size = ptr_type.size();
            let val = context.get_value(value).unwrap();

            let ptr_diff = context
                .builder
                .ins()
                .imul_imm(val.as_value(), Imm64::from(size as i64));

            CodegenValue::Value(ptr_diff).into()
        }

        BCInstructionKind::IntToPtr { value } => context.get_value(value),

        BCInstructionKind::PointerBinOp {
            op, left, right, ..
        } => {
            let left = context.get_value(left).unwrap().as_value();
            let right = context.get_value(right).unwrap().as_value();
            let _type = &instruction.value_type;

            let inst =
                match op {
                    BCPtrBinOp::ADD => context.builder.ins().iadd(left, right),
                    BCPtrBinOp::SUB => context.builder.ins().isub(left, right),

                    BCPtrBinOp::EQ => {
                        context
                            .builder
                            .ins()
                            .icmp(ir::condcodes::IntCC::Equal, left, right)
                    }
                    BCPtrBinOp::NE => {
                        context
                            .builder
                            .ins()
                            .icmp(ir::condcodes::IntCC::NotEqual, left, right)
                    }

                    BCPtrBinOp::LT => context.builder.ins().icmp(
                        ir::condcodes::IntCC::SignedLessThan,
                        left,
                        right,
                    ),
                    BCPtrBinOp::GT => context.builder.ins().icmp(
                        ir::condcodes::IntCC::SignedGreaterThan,
                        left,
                        right,
                    ),
                    BCPtrBinOp::LE => context.builder.ins().icmp(
                        ir::condcodes::IntCC::SignedLessThanOrEqual,
                        left,
                        right,
                    ),
                    BCPtrBinOp::GE => context.builder.ins().icmp(
                        ir::condcodes::IntCC::SignedGreaterThanOrEqual,
                        left,
                        right,
                    ),
                };

            Some(CodegenValue::Value(inst))
        }

        BCInstructionKind::IntegerBinOp { op, left, right } => {
            let left = context.get_value(left).unwrap().as_value();
            let right = context.get_value(right).unwrap().as_value();

            let inst = match op {
                BCIntBinOp::ADD => context.builder.ins().iadd(left, right),
                BCIntBinOp::SUB => context.builder.ins().isub(left, right),
                BCIntBinOp::MUL => context.builder.ins().imul(left, right),
                BCIntBinOp::IDIV => context.builder.ins().sdiv(left, right),
                BCIntBinOp::IREM => context.builder.ins().srem(left, right),

                BCIntBinOp::UDIV => context.builder.ins().udiv(left, right),
                BCIntBinOp::UREM => context.builder.ins().urem(left, right),

                BCIntBinOp::SHL => context.builder.ins().ishl(left, right),
                BCIntBinOp::ASHR => context.builder.ins().sshr(left, right),
                BCIntBinOp::LSHR => context.builder.ins().ushr(left, right),

                BCIntBinOp::BAND => context.builder.ins().band(left, right),
                BCIntBinOp::BOR => context.builder.ins().bor(left, right),
                BCIntBinOp::BXOR => context.builder.ins().bxor(left, right),

                BCIntBinOp::LAND => {
                    let left = context
                        .builder
                        .ins()
                        .icmp_imm(ir::condcodes::IntCC::Equal, left, 0);
                    let right =
                        context
                            .builder
                            .ins()
                            .icmp_imm(ir::condcodes::IntCC::Equal, right, 0);

                    context.builder.ins().band(left, right)
                }

                BCIntBinOp::LOR => {
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

                BCIntBinOp::EQ => {
                    context
                        .builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::Equal, left, right)
                }
                BCIntBinOp::NE => {
                    context
                        .builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::NotEqual, left, right)
                }

                BCIntBinOp::ILT => {
                    context
                        .builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::SignedLessThan, left, right)
                }
                BCIntBinOp::IGT => {
                    context
                        .builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::SignedGreaterThan, left, right)
                }
                BCIntBinOp::ILE => context.builder.ins().icmp(
                    ir::condcodes::IntCC::SignedLessThanOrEqual,
                    left,
                    right,
                ),
                BCIntBinOp::IGE => context.builder.ins().icmp(
                    ir::condcodes::IntCC::SignedGreaterThanOrEqual,
                    left,
                    right,
                ),

                BCIntBinOp::ULT => {
                    context
                        .builder
                        .ins()
                        .icmp(ir::condcodes::IntCC::UnsignedLessThan, left, right)
                }
                BCIntBinOp::UGT => context.builder.ins().icmp(
                    ir::condcodes::IntCC::UnsignedGreaterThan,
                    left,
                    right,
                ),
                BCIntBinOp::ULE => context.builder.ins().icmp(
                    ir::condcodes::IntCC::UnsignedLessThanOrEqual,
                    left,
                    right,
                ),
                BCIntBinOp::UGE => context.builder.ins().icmp(
                    ir::condcodes::IntCC::UnsignedGreaterThanOrEqual,
                    left,
                    right,
                ),
            };

            Some(CodegenValue::Value(inst))
        }

        BCInstructionKind::IntegerUnOp { op, value } => {
            let val = context.get_value(value).unwrap();

            let inst = match op {
                BCIntUnOp::NEG => context.builder.ins().ineg(val.as_value()),
                BCIntUnOp::BNOT => context.builder.ins().bnot(val.as_value()),
                BCIntUnOp::LNOT => {
                    context
                        .builder
                        .ins()
                        .icmp_imm(ir::condcodes::IntCC::Equal, val.as_value(), 0)
                }
            };

            Some(CodegenValue::Value(inst))
        }

        BCInstructionKind::FloatUnOp { value, op } => {
            let val = context.get_value(value).unwrap();
            let _type = &instruction.value_type;

            match op {
                BCFloatUnOp::NEG => Some(CodegenValue::Value(
                    context.builder.ins().fneg(val.as_value()),
                )),
            }
        }

        BCInstructionKind::FloatBinOp { left, right, op } => {
            let left = context.get_value(left).unwrap().as_value();
            let right = context.get_value(right).unwrap().as_value();

            Some(CodegenValue::Value(match op {
                BCFloatBinOp::ADD => context.builder.ins().fadd(left, right),
                BCFloatBinOp::SUB => context.builder.ins().fsub(left, right),
                BCFloatBinOp::FMUL => context.builder.ins().fmul(left, right),
                BCFloatBinOp::FDIV => context.builder.ins().fdiv(left, right),
            }))
        }

        BCInstructionKind::Branch {
            condition,
            true_block,
            false_block,
        } => {
            let condition = context.get_value(condition).unwrap().as_value();
            let true_block = context.get_block(*true_block);
            let false_block = context.get_block(*false_block);

            context
                .builder
                .ins()
                .brif(condition, true_block, &[], false_block, &[]);

            Some(CodegenValue::NULL)
        }

        BCInstructionKind::GotoDefer => {
            let defer_block = context
                .block_map
                .get(context.defer_offset)
                .expect("Defer block not found in block map");

            context.builder.ins().jump(*defer_block, &[]);

            Some(CodegenValue::NULL)
        }

        BCInstructionKind::Jump { target } => {
            let target = context.get_block(*target);

            context.builder.ins().jump(target, &[]);

            Some(CodegenValue::NULL)
        }

        BCInstructionKind::StructAccess {
            struct_,
            field_offset,
            ..
        } => {
            let ptr = context.get_value(struct_).unwrap().clone();
            let _type = &instruction.value_type;

            Some(CodegenValue::Value(
                context
                    .builder
                    .ins()
                    .iadd_imm(ptr.as_value(), *field_offset as i64),
            ))
        }

        BCInstructionKind::Store {
            memory,
            value,
            _type,
        } => {
            let target = context.get_value(memory).unwrap().as_value();
            let value = context.get_value(value).unwrap().as_value();

            if _type.is_structure() {
                let size = _type.size();
                let size_literal = context
                    .builder
                    .ins()
                    .iconst(ir::Type::int(64).unwrap(), size as i64);

                context.builder.call_memcpy(
                    *context.target_frontend_config,
                    target,
                    value,
                    size_literal,
                )
            } else {
                context
                    .builder
                    .ins()
                    .store(MemFlags::new(), value, target, 0);
            }

            Some(CodegenValue::NULL)
        }

        BCInstructionKind::ZeroMemory { memory, _type } => {
            let target = context.get_value(memory).unwrap().as_value();

            let size_literal = match _type.size() {
                MIRTypeSize::Fixed(size) => context
                    .builder
                    .ins()
                    .iconst(ir::Type::int(64).unwrap(), size as i64),
                MIRTypeSize::Variable(size_expr) => {
                    let size_value = context.get_value(&size_expr).unwrap();
                    context
                        .builder
                        .ins()
                        .uextend(ir::Type::int(64).unwrap(), size_value.as_value())
                }
            };

            let zero = context.builder.ins().iconst(ir::Type::int(8).unwrap(), 0);

            context.builder.call_memset(
                *context.target_frontend_config,
                target,
                zero,
                size_literal,
            );

            Some(CodegenValue::NULL)
        }

        BCInstructionKind::Phi { predecessors } => {
            let current_block = context.builder.current_block()?;
            let current_block_len = context
                .builder
                .func
                .layout
                .block_insts(current_block)
                .count();

            context
                .builder
                .append_block_param(current_block, get_cranelift_type(&instruction.value_type));

            for (from_value, from_block) in predecessors {
                let block = context.get_block(*from_block);

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

                let value = context.get_value(from_value).unwrap().as_value();

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
                            if blocks.get_mut(0)?.block(&*value_pool) == current_block {
                                blocks.get_mut(0)?.append_argument(value, &mut *value_pool);
                            }
                            if blocks.get_mut(1)?.block(&*value_pool) == current_block {
                                blocks.get_mut(1)?.append_argument(value, &mut *value_pool);
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

            Some(CodegenValue::Value(val))
        }

        BCInstructionKind::BoolExtend { value } => {
            let val = context.get_value(value).unwrap();

            match instruction.value_type.kind {
                BCTypeKind::Signed { bytes: 1 } | BCTypeKind::Integer { bytes: 1 } => Some(val),

                _ => {
                    let _type = &instruction.value_type;
                    let cranelift_type = get_cranelift_type(_type);

                    Some(CodegenValue::Value(
                        context
                            .builder
                            .ins()
                            .uextend(cranelift_type, val.as_value()),
                    ))
                }
            }
        }

        BCInstructionKind::ZExtend { value } => {
            let val = context.get_value(value).unwrap().as_value();
            let _type = &instruction.value_type;
            let cranelift_type = get_cranelift_type(_type);

            let val_type = context.builder.func.dfg.value_type(val);

            if val_type == cranelift_type {
                return Some(CodegenValue::Value(val));
            }

            Some(CodegenValue::Value(
                context.builder.ins().uextend(cranelift_type, val),
            ))
        }

        BCInstructionKind::SExtend { value } => {
            let val = context.get_value(value).unwrap().as_value();

            let _type = &instruction.value_type;
            let cranelift_type = get_cranelift_type(_type);

            let val_type = context.builder.func.dfg.value_type(val);

            if val_type == cranelift_type {
                return Some(CodegenValue::Value(val));
            }

            Some(CodegenValue::Value(
                context.builder.ins().sextend(cranelift_type, val),
            ))
        }

        BCInstructionKind::Trunc { value } => {
            let val = context.get_value(value).unwrap();
            let _type = &instruction.value_type;

            let value = val.as_value();
            let cranelift_type = get_cranelift_type(_type);
            let value_type = context.builder.func.dfg.value_type(value);

            if value_type == cranelift_type {
                return Some(CodegenValue::Value(value));
            }

            Some(CodegenValue::Value(
                context
                    .builder
                    .ins()
                    .ireduce(cranelift_type, val.as_value()),
            ))
        }

        BCInstructionKind::NOP => Some(CodegenValue::NULL),

        BCInstructionKind::BitCast { value } => {
            let Some(val) = context.get_value(value) else {
                panic!("Value not found for BitCast: {value:?}");
            };

            Some(val)
        }

        BCInstructionKind::FloatCast { value } => {
            let val = context.get_value(value).unwrap();
            let to_type = &instruction.value_type;
            let cranelift_type = get_cranelift_type(to_type);

            match &to_type.kind {
                BCTypeKind::Float { bytes } if *bytes == 4 => Some(CodegenValue::Value(
                    context
                        .builder
                        .ins()
                        .fdemote(cranelift_type, val.as_value()),
                )),
                BCTypeKind::Float { bytes } if *bytes == 8 => Some(CodegenValue::Value(
                    context
                        .builder
                        .ins()
                        .fpromote(cranelift_type, val.as_value()),
                )),
                _ => unreachable!("Invalid type for float cast"),
            }
        }

        BCInstructionKind::IntToFloat { from, value } => {
            let val = context.get_value(value).unwrap().as_value();
            let _type = &instruction.value_type;

            let to_cl_type = get_cranelift_type(_type);

            let inst = if matches!(from.kind, BCTypeKind::Signed { .. }) {
                context.builder.ins().fcvt_from_sint(to_cl_type, val)
            } else if matches!(from.kind, BCTypeKind::Integer { .. }) {
                context.builder.ins().fcvt_from_uint(to_cl_type, val)
            } else {
                panic!("Invalid type for int to float conversion")
            };

            Some(CodegenValue::Value(inst))
        }

        BCInstructionKind::FloatToInt { from, value } => {
            let val = context.get_value(value).unwrap().as_value();
            let _type = &instruction.value_type;

            let to_cl_type = get_cranelift_type(_type);

            let BCTypeKind::Float {
                bytes: float_bytes, ..
            } = &from.kind
            else {
                panic!("Invalid type for float to int conversion")
            };

            let (ival, signed, int_bytes) =
                if let BCTypeKind::Signed { bytes: int_bytes } = &_type.kind {
                    let ival = context.builder.ins().fcvt_to_sint(to_cl_type, val);
                    (ival, true, int_bytes)
                } else if let BCTypeKind::Integer { bytes: int_bytes } = &_type.kind {
                    let ival = context.builder.ins().fcvt_to_uint(to_cl_type, val);
                    (ival, false, int_bytes)
                } else {
                    panic!("Invalid type for float to int conversion")
                };

            let val = if float_bytes > int_bytes {
                context.builder.ins().ireduce(to_cl_type, ival)
            } else if float_bytes < int_bytes && signed {
                context.builder.ins().sextend(to_cl_type, ival)
            } else if float_bytes < int_bytes {
                context.builder.ins().uextend(to_cl_type, ival)
            } else {
                ival
            };

            Some(CodegenValue::Value(val))
        }

        BCInstructionKind::CompilerAssertion { condition, message } => {
            let cond = context.get_value(condition).unwrap().as_value();
            let message_ref = context.get_value(message)?.as_value();

            let assert_prototype = MIRFunctionPrototype {
                name: "__compiler_assert".to_string(),
                params: vec![
                    MIRParameter {
                        name: None,
                        _type: BCTypeKind::Bool.into(),
                    },
                    MIRParameter {
                        name: None,
                        _type: BCType::default_pointer(),
                    },
                ],
                return_type: BCType::unit(),
                var_args: false,
                linkage: LinkageType::External,
            };

            let Some(func_id) = get_function(context, &assert_prototype) else {
                panic!("Failed to get function id for compiler assertion");
            };

            let func_ref =
                get_func_ref(context, func_id, &assert_prototype, &[cond, message_ref]).unwrap();

            context.builder.ins().call(func_ref, &[cond, message_ref]);

            Some(CodegenValue::NULL)
        }

        BCInstructionKind::JumpTable {
            value,
            targets,
            default,
        } => {
            let mut switch = Switch::new();

            for (value, block_id) in targets {
                switch.set_entry(*value as u128, context.get_block(*block_id));
            }

            let default_block = context.get_block(*default);
            let value = context.get_value(value).unwrap();

            switch.emit(&mut context.builder, value.as_value(), default_block);

            Some(CodegenValue::NULL)
        }
    }
}
