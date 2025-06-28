use crate::inst_calling::{get_func_ref, get_method_return, prepare_method_call};
use crate::routines::allocate_variable;
use crate::value_type::{get_cranelift_abi_type, get_cranelift_type};
use crate::{CodegenValue, FunctionState};
use cranelift::codegen::ir;
use cranelift::codegen::ir::DynamicStackSlotData;
use cranelift::codegen::ir::stackslot::StackSize;
use cranelift::frontend::Switch;
use cranelift::prelude::{Imm64, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Value};
use cranelift_module::Module;
use cx_data_ast::parse::value_type::CXTypeKind;
use cx_data_bytecode::{BCFloatBinOp, BCIntBinOp, BCIntUnOp, BlockInstruction, BCFunctionPrototype, ValueID, VirtualInstruction, BCPtrBinOp, BCFloatUnOp};
use cx_data_bytecode::types::BCTypeKind;

pub(crate) fn codegen_instruction(context: &mut FunctionState, instruction: &BlockInstruction) -> Option<CodegenValue> {
    match &instruction.instruction {
        VirtualInstruction::Allocate {
            size, alignment
        } => {
            let slot = context.builder.create_sized_stack_slot(
                StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    StackSize::from(*size as u32),
                    *alignment
                )
            );

            Some(
                CodegenValue::Value(
                    context.builder.ins().stack_addr(
                        context.pointer_type,
                        slot,
                        0
                    )
                )
            )
        },

        VirtualInstruction::VariableAllocate {
            ..
        } => unimplemented!("The Cranelift backend does not currently support dynamic stack allocation, \
                             use the LLVM backend if this feature is desired."),

        VirtualInstruction::StringLiteral { str_id } => {
            let global_id = context.global_strs.get(*str_id as usize).cloned().unwrap();
            let global_val = context.object_module.declare_data_in_func(global_id, &mut context.builder.func);

            Some(
                CodegenValue::Value(
                    context.builder.ins()
                        .global_value(context.pointer_type, global_val)
                )
            )
        },

        VirtualInstruction::DirectCall {
            func, args, method_sig
        } => {
            let (val, params) = prepare_method_call(
                context,
                func.clone(),
                method_sig,
                args
            )?;

            let fn_ref = get_func_ref(
                context,
                val.as_func_id(),
                val.as_func_name(),
                params.as_slice()
            )?;

            let inst = context.builder.ins()
                .call(fn_ref, params.as_slice());

            get_method_return(context, inst)
        },

        VirtualInstruction::IndirectCall {
            func_ptr, args, method_sig
        } => {
            let (val, params) = prepare_method_call(
                context,
                func_ptr.clone(),
                &context.function_prototype,
                args
            )?;

            let mut sig = context.object_module.make_signature();
            let return_type = &method_sig.return_type;

            sig.returns = if return_type.is_void() {
                vec![]
            } else {
                vec![get_cranelift_abi_type(return_type)]
            };
            sig.params = method_sig.params
                .iter()
                .map(|arg| get_cranelift_abi_type(&arg.type_))
                .collect();

            for i in method_sig.params.len()..params.len() {
                let arg = params[i];
                let arg_type = context.builder.func.dfg.value_type(arg);

                sig.params.push(ir::AbiParam::new(arg_type));
            }

            let sig_ref = context.builder.import_signature(sig);

            let inst = context.builder.ins().call_indirect(
                sig_ref, val.as_value(), params.as_slice()
            );

            get_method_return(context, inst)
        },

        VirtualInstruction::FunctionReference {
            name
        } => {
            Some(
                CodegenValue::FunctionID {
                    fn_name: name.clone(),
                    id: context.function_ids.get(name).cloned().unwrap()
                }
            )
        }

        VirtualInstruction::Return { value } => {
            match value {
                Some(value) => {
                    let return_value = context.variable_table.get(value).cloned().unwrap();

                    if context.function_prototype.return_type.is_structure() {
                        let size = context.function_prototype.return_type.fixed_size();
                        let size_literal = context.builder.ins().iconst(ir::Type::int(64).unwrap(), size as i64);
                        let callee_buffer = Value::from_u32(0);

                        context.builder.call_memcpy(
                            context.target_frontend_config.clone(),
                            callee_buffer,
                            return_value.as_value(),
                            size_literal
                        );

                        context.builder.ins().return_(&[callee_buffer]);
                    } else {
                        context.builder.ins().return_(&[return_value.as_value()]);
                    }
                },
                None => {
                    context.builder.ins().return_(&[]);
                },
            };

            None
        },

        VirtualInstruction::FunctionParameter { param_index, .. } => {
            let parameter_ptr = context.fn_params.get(*param_index as usize).cloned().unwrap();

            Some(
                CodegenValue::Value(
                    parameter_ptr
                )
            )
        },

        VirtualInstruction::GetFunctionAddr {
            func_name
        } => {
            let CodegenValue::FunctionID { id, .. }
                = context.variable_table.get(func_name).cloned().unwrap() else {
                panic!("Function reference not found")
            };
            let func_ref = context.object_module.declare_func_in_func(
                id,
                &mut context.builder.func
            );

            let pointer = context.pointer_type.clone();
            Some(
                CodegenValue::Value(
                    context.builder.ins()
                        .func_addr(pointer, func_ref)
                )
            )
        },

        VirtualInstruction::Load { value } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let type_ = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(type_);

            Some(
                CodegenValue::Value(
                    context.builder.ins()
                        .load(
                            cranelift_type,
                            MemFlags::new(),
                            val.as_value(),
                            0
                        )
                )
            )
        },
        
        VirtualInstruction::PtrToInt { 
            value
        } => {
            let bytes = match instruction.value.type_.kind {
                BCTypeKind::Signed { bytes, .. } => bytes,
                BCTypeKind::Unsigned { bytes, .. } => bytes,
                _ => panic!("Invalid type for pointer to integer conversion")
            };
            
            let val = context.variable_table.get(value).cloned().unwrap();
            
            if bytes < 8 {
                return Some(
                    CodegenValue::Value(
                        context.builder.ins().ireduce(
                            ir::Type::int(bytes as u16).unwrap(),
                            val.as_value()
                        )
                    )
                );
            };
            
            Some(val)
        },
        
        VirtualInstruction::IntToPtrDiff { value, ptr_type } => {
            let size = ptr_type.fixed_size();
            let val = context.variable_table.get(value).cloned().unwrap();
            
            let ptr_diff = context.builder.ins().imul_imm(
                val.as_value(),
                Imm64::from(size as i64)
            );
            
            CodegenValue::Value(ptr_diff).into()
        }, 
        
        VirtualInstruction::PointerBinOp {
            op, left, right, ..
        } => {
            let left = context.variable_table.get(left).unwrap().as_value();
            let right = context.variable_table.get(right).unwrap().as_value();
            let _type = &instruction.value.type_;

            let inst = match op {
                BCPtrBinOp::ADD => context.builder.ins().iadd(left, right),
                BCPtrBinOp::SUB => context.builder.ins().isub(left, right),
                
                BCPtrBinOp::EQ  => context.builder.ins().icmp(ir::condcodes::IntCC::Equal, left, right),
                BCPtrBinOp::NE  => context.builder.ins().icmp(ir::condcodes::IntCC::NotEqual, left, right),
                
                BCPtrBinOp::LT  => context.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThan, left, right),
                BCPtrBinOp::GT  => context.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThan, left, right),
                BCPtrBinOp::LE  => context.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThanOrEqual, left, right),
                BCPtrBinOp::GE  => context.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThanOrEqual, left, right),
            };

            Some(
                CodegenValue::Value(
                    inst
                )
            )
        },

        VirtualInstruction::IntegerBinOp {
            op, left, right
        } => {
            let left = context.variable_table.get(left).unwrap().as_value();
            let right = context.variable_table.get(right).unwrap().as_value();

            let inst = match op {
                BCIntBinOp::ADD             => context.builder.ins().iadd(left, right),
                BCIntBinOp::SUB             => context.builder.ins().isub(left, right),
                BCIntBinOp::MUL             => context.builder.ins().imul(left, right),
                BCIntBinOp::IDIV            => context.builder.ins().sdiv(left, right),
                BCIntBinOp::IREM            => context.builder.ins().srem(left, right),
                
                BCIntBinOp::UDIV            => context.builder.ins().udiv(left, right),
                BCIntBinOp::UREM            => context.builder.ins().urem(left, right),
                
                BCIntBinOp::SHL             => context.builder.ins().ishl(left, right),
                BCIntBinOp::ASHR            => context.builder.ins().sshr(left, right),
                BCIntBinOp::LSHR            => context.builder.ins().ushr(left, right),
                
                BCIntBinOp::BAND            => context.builder.ins().band(left, right),
                BCIntBinOp::BOR             => context.builder.ins().bor(left, right),
                BCIntBinOp::BXOR            => context.builder.ins().bxor(left, right),
                
                BCIntBinOp::LAND            => {
                    let left = context.builder.ins().icmp_imm(ir::condcodes::IntCC::Equal, left, 0);
                    let right = context.builder.ins().icmp_imm(ir::condcodes::IntCC::Equal, right, 0);

                    context.builder.ins().band(left, right)
                },
                
                BCIntBinOp::LOR             => {
                    let left = context.builder.ins().icmp_imm(ir::condcodes::IntCC::Equal, left, 0);
                    let right = context.builder.ins().icmp_imm(ir::condcodes::IntCC::Equal, right, 0);

                    context.builder.ins().bor(left, right)
                },
                
                BCIntBinOp::EQ              => context.builder.ins().icmp(ir::condcodes::IntCC::Equal, left, right),
                BCIntBinOp::NE              => context.builder.ins().icmp(ir::condcodes::IntCC::NotEqual, left, right),
                
                BCIntBinOp::ILT             => context.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThan, left, right),
                BCIntBinOp::IGT             => context.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThan, left, right),
                BCIntBinOp::ILE             => context.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThanOrEqual, left, right),
                BCIntBinOp::IGE             => context.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThanOrEqual, left, right),
                
                BCIntBinOp::ULT             => context.builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThan, left, right),
                BCIntBinOp::UGT             => context.builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThan, left, right),
                BCIntBinOp::ULE             => context.builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThanOrEqual, left, right),
                BCIntBinOp::UGE             => context.builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThanOrEqual, left, right),
                
                _ => unimplemented!("Operator not implemented: {:?}", op)
            };

            Some(
                CodegenValue::Value(
                    inst
                )
            )
        },
        
        VirtualInstruction::IntegerUnOp {
            op, value
        } => {
            let val = context.variable_table.get(value).cloned().unwrap();

            let inst = match op {
                BCIntUnOp::NEG      => context.builder.ins().ineg(val.as_value()),
                BCIntUnOp::BNOT     => context.builder.ins().bnot(val.as_value()),
                BCIntUnOp::LNOT     => context.builder.ins().icmp_imm(ir::condcodes::IntCC::Equal, val.as_value(), 0),
                _ => todo!("UnOp not implemented: {:?}", op)
            };

            Some(
                CodegenValue::Value(
                    inst
                )
            )
        }
        
        VirtualInstruction::FloatUnOp { value, op } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let _type = &instruction.value.type_;
            
            match op {
                BCFloatUnOp::NEG => {
                    Some(
                        CodegenValue::Value(
                            context.builder.ins().fneg(val.as_value())
                        )
                    )
                }
            }
        }

        VirtualInstruction::FloatBinOp {
            left, right, op
        } => {
            let left = context.variable_table.get(left).unwrap().as_value();
            let right = context.variable_table.get(right).unwrap().as_value();

            Some(
                CodegenValue::Value(
                    match op {
                        BCFloatBinOp::ADD           => context.builder.ins().fadd(left, right),
                        BCFloatBinOp::SUB           => context.builder.ins().fsub(left, right),
                        BCFloatBinOp::FMUL          => context.builder.ins().fmul(left, right),
                        BCFloatBinOp::FDIV          => context.builder.ins().fdiv(left, right),
                        
                        _ => unimplemented!("Operator not implemented: {:?}", op)
                    }
                )
            )
        },

        VirtualInstruction::Branch {
            condition, true_block, false_block
        } => {
            let condition = context.variable_table.get(condition).unwrap().as_value();
            let true_block = context.block_map.get(*true_block as usize).unwrap().clone();
            let false_block = context.block_map.get(*false_block as usize).unwrap().clone();

            context.builder.ins()
                .brif(condition,
                      true_block, &[],
                      false_block, &[]);

            Some(CodegenValue::NULL)
        },

        VirtualInstruction::Jump {
            target
        } => {
            let target = context.block_map.get(*target as usize).unwrap().clone();

            context.builder.ins().jump(target, &[]);

            Some(CodegenValue::NULL)
        },

        VirtualInstruction::StructAccess {
            struct_, field_offset, ..
        } => {
            let ptr = context.variable_table.get(struct_).unwrap().clone();
            let _type = &instruction.value.type_;

            Some(
                CodegenValue::Value(
                    context.builder.ins().iadd_imm(ptr.as_value(), *field_offset as i64)
                )
            )
        },

        VirtualInstruction::Store {
            memory, value, type_
        } => {
            let target = context.variable_table.get(memory)
                .unwrap()
                .as_value();
            let value = context.variable_table.get(value)
                .unwrap()
                .as_value();

            if type_.is_structure() {
                let size = type_.fixed_size();
                let size_literal = context.builder.ins().iconst(ir::Type::int(64).unwrap(), size as i64);

                context.builder.call_memcpy(
                    context.target_frontend_config.clone(),
                    target,
                    value,
                    size_literal
                )
            } else {
                context.builder.ins().store(MemFlags::new(), value, target, 0);
            }

            Some(CodegenValue::NULL)
        },


        VirtualInstruction::ZExtend {
            value
        } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let _type = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(_type);

            Some(
                CodegenValue::Value(
                    context.builder.ins().uextend(cranelift_type, val.as_value())
                )
            )
        },

        VirtualInstruction::SExtend {
            value
        } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let _type = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(_type);

            Some(
                CodegenValue::Value(
                    context.builder.ins().sextend(cranelift_type, val.as_value())
                )
            )
        }

        VirtualInstruction::Trunc {
            value
        } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let _type = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(_type);

            Some(
                CodegenValue::Value(
                    context.builder.ins().ireduce(cranelift_type, val.as_value())
                )
            )
        }

        VirtualInstruction::NOP => Some(CodegenValue::NULL),

        VirtualInstruction::Immediate {
            value
        } => {
            let _type = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(_type);

            Some(
                CodegenValue::Value(
                    context.builder.ins().iconst(cranelift_type, *value as i64)
                )
            )
        },
        
        VirtualInstruction::FloatImmediate { 
            value
        } => {
            let BCTypeKind::Float { bytes } = &instruction.value.type_.kind
                else { unreachable!("Non-FP Type Float Immediate") };

            Some(
                CodegenValue::Value(
                    match bytes {
                        4 => context.builder.ins().f32const(*value as f32),
                        8 => context.builder.ins().f64const(*value),
                        
                        _ => panic!("Unsupported float size: {}", bytes)
                    }
                )
            )
        }

        VirtualInstruction::BitCast {
            value
        } => {
            let val = context.variable_table.get(value).cloned().unwrap();

            Some(val)
        },
        
        VirtualInstruction::FloatCast { value } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let to_type = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(to_type);
            
            match &to_type.kind {
                BCTypeKind::Float { bytes }
                    if *bytes == 4 => {
                        Some(
                            CodegenValue::Value(
                                context.builder.ins().fdemote(cranelift_type, val.as_value())
                            )
                        )
                    },
                BCTypeKind::Float { bytes }
                    if *bytes == 8 => {
                        Some(
                            CodegenValue::Value(
                                context.builder.ins().fpromote(cranelift_type, val.as_value())
                            )
                        )
                    },
                _ => unreachable!("Invalid type for float cast")
            }
        }, 

        VirtualInstruction::IntToFloat {
            from, value
        } => {
            let val = context.variable_table.get(value).unwrap().as_value();
            let _type = &instruction.value.type_;

            let to_cl_type = get_cranelift_type(_type);

            let inst = if matches!(from.kind, BCTypeKind::Signed { .. }) {
                context.builder.ins().fcvt_from_sint(to_cl_type, val)
            } else if matches!(from.kind, BCTypeKind::Unsigned { .. }) {
                context.builder.ins().fcvt_from_uint(to_cl_type, val)
            } else {
                panic!("Invalid type for int to float conversion")
            };

            Some(
                CodegenValue::Value(
                    inst
                )
            )
        },

        VirtualInstruction::FloatToInt {
            from, value
        } => {
            let val = context.variable_table.get(value).unwrap().as_value();
            let _type = &instruction.value.type_;

            let to_cl_type = get_cranelift_type(_type);

            let BCTypeKind::Float { bytes: float_bytes, .. } = &from.kind else {
                panic!("Invalid type for float to int conversion")
            };

            let (ival, signed, int_bytes) =
            if let BCTypeKind::Signed { bytes: int_bytes } = &_type.kind {
                let ival = context.builder.ins().fcvt_to_sint(to_cl_type, val);
                (ival, true, int_bytes)
            } else if let BCTypeKind::Unsigned { bytes: int_bytes } = &_type.kind {
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

            Some(
                CodegenValue::Value(
                    val
                )
            )
        },
        
        VirtualInstruction::JumpTable { value, targets, default } => {
            let mut switch = Switch::new();
            
            for (value, block_id) in targets {
                switch.set_entry(
                    *value as u128,
                    context.block_map.get(*block_id as usize).unwrap().clone()
                );
            }
            
            switch.emit(
                &mut context.builder,
                context.variable_table.get(value).unwrap().as_value(),
                context.block_map.get(*default as usize).unwrap().clone()
            );
            
            Some(CodegenValue::NULL)
        }
    }
}

fn generate_params(
    context: &mut FunctionState,
    prototype: &BCFunctionPrototype,
    params: &[ValueID]
) -> Option<Vec<Value>> {
    let mut args = Vec::new();

    if prototype.return_type.is_structure() {
        let temp_buffer = allocate_variable(
            context,
            prototype.return_type.fixed_size() as u32,
            None
        )?;

        args.push(temp_buffer);
    }

    for param in params.iter() {
        args.push(
            context.variable_table.get(param).unwrap().as_value()
        );
    }

    Some(args)
}