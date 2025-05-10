use cranelift::codegen::gimli::ReaderOffset;
use cranelift::codegen::ir;
use cranelift::codegen::ir::stackslot::StackSize;
use cranelift::codegen::ir::FuncRef;
use cranelift::prelude::{Block, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Value};
use cranelift_module::Module;
use cx_data_ast::parse::ast::CXBinOp;
use cx_data_ast::parse::value_type::{get_intrinsic_type, get_type_size, is_structure};
use cx_data_bytecode::builder::{BlockInstruction, BytecodeFunctionPrototype, ValueID, VirtualInstruction};
use crate::FunctionState;
use crate::routines::allocate_variable;
use crate::value_type::get_cranelift_type;

/**
 *  May or may not return a valid, panics if error occurs
 */
pub(crate) fn codegen_instruction(context: &mut FunctionState, instruction: &BlockInstruction) -> Option<Value> {
    match &instruction.instruction {
        VirtualInstruction::Allocate {
            size
        } => {
            let slot = context.builder.create_sized_stack_slot(
                StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    StackSize::from(*size as u32),
                    0u8
                )
            );

            Some(
                context.builder.ins().stack_addr(
                    context.pointer_type,
                    slot,
                    0
                )
            )
        }

        VirtualInstruction::Literal {
            val
        } => Some(context.builder.ins().iconst(ir::Type::int(32)?, *val as i64)),

        VirtualInstruction::StringLiteral { str_id } => {
            let global_id = context.global_strs.get(*str_id as usize).cloned().unwrap();
            let global_val = context.object_module.declare_data_in_func(global_id, &mut context.builder.func);

            Some(
                context.builder.ins()
                    .global_value(context.pointer_type, global_val)
            )
        },

        VirtualInstruction::MethodCall {
            func, args
        } => {
            let val = context.variable_table.get(func).cloned().unwrap();
            let mut params = args.iter()
                .map(|arg| context.variable_table.get(arg).cloned().unwrap())
                .collect::<Vec<_>>();

            if is_structure(context.type_map, &instruction.value.type_) {
                let type_size = get_type_size(context.type_map, &context.function_prototype.return_type)?;

                let temp_buffer = allocate_variable(context, type_size as u32, None)?;

                params.insert(0, temp_buffer);
            }

            let inst = context.builder.ins().call(
                FuncRef::from_u32(val.as_u32()),
                params.as_slice()
            );

            let returns = context.builder.inst_results(inst);

            returns.first().cloned().or(Some(Value::from_u32(0)))
        }

        VirtualInstruction::FunctionReference {
            name
        } => {
            if let Some(func_ref) = context.local_defined_functions.get(name) {
                return Some(
                    Value::from_u32(func_ref.as_u32())
                );
            }

            let func_id = context.function_ids.get(name).unwrap();
            let func_ref = context.object_module.declare_func_in_func(
                *func_id,
                &mut context.builder.func
            );

            context.local_defined_functions.insert(name.clone(), func_ref);

            Some(
                Value::from_u32(func_ref.as_u32())
            )
        }

        VirtualInstruction::Return { value } => {
            match value {
                Some(value) => {
                    let return_value = context.variable_table.get(value).cloned().unwrap();

                    if is_structure(context.type_map, &context.function_prototype.return_type) {
                        let size = get_type_size(context.type_map, &context.function_prototype.return_type).unwrap() as u64;
                        let size_literal = context.builder.ins().iconst(ir::Type::int(64).unwrap(), size as i64);
                        let callee_buffer = Value::from_u32(0);

                        context.builder.call_memcpy(
                            context.target_frontend_config.clone(),
                            callee_buffer,
                            return_value,
                            size_literal
                        );

                        context.builder.ins().return_(&[callee_buffer]);
                    } else {
                        context.builder.ins().return_(&[return_value]);
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

            Some(parameter_ptr)
        },

        VirtualInstruction::Load { value } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let type_ = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(type_, context.type_map);

            Some(
                context.builder.ins()
                    .load(
                        cranelift_type,
                        MemFlags::new(),
                        val,
                        0
                    )
            )
        },

        VirtualInstruction::IntegerBinOp {
            op, left, right
        } => {
            let left = context.variable_table.get(left).cloned().unwrap();
            let right = context.variable_table.get(right).cloned().unwrap();

            let inst = match op {
                CXBinOp::Add                => context.builder.ins().iadd(left, right),
                CXBinOp::Subtract           => context.builder.ins().isub(left, right),
                CXBinOp::Multiply           => context.builder.ins().imul(left, right),
                CXBinOp::Divide             => context.builder.ins().udiv(left, right),
                CXBinOp::Modulus            => context.builder.ins().urem(left, right),

                CXBinOp::Less          => context.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThan, left, right),
                CXBinOp::Greater       => context.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThan, left, right),
                CXBinOp::LessEqual     => context.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThanOrEqual, left, right),
                CXBinOp::GreaterEqual  => context.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThanOrEqual, left, right),
                CXBinOp::Equal         => context.builder.ins().icmp(ir::condcodes::IntCC::Equal, left, right),
                CXBinOp::NotEqual      => context.builder.ins().icmp(ir::condcodes::IntCC::NotEqual, left, right),

                CXBinOp::LAnd          => {
                    let left = context.builder.ins().icmp_imm(ir::condcodes::IntCC::Equal, left, 0);
                    let right = context.builder.ins().icmp_imm(ir::condcodes::IntCC::Equal, right, 0);

                    context.builder.ins().band(left, right)
                }

                _ => unimplemented!("Operator not implemented: {:?}", op)
            };

            Some(inst)
        },

        VirtualInstruction::Branch {
            condition, true_block, false_block
        } => {
            let condition = context.variable_table.get(condition).cloned().unwrap();
            let true_block = Block::from_u32(*true_block + 1);
            let false_block = Block::from_u32(*false_block + 1);

            context.builder.ins()
                .brif(condition,
                      true_block, &[],
                      false_block, &[]);

            Some(Value::from_u32(0))
        },

        VirtualInstruction::Jump {
            target
        } => {
            let target = Block::from_u32(*target + 1);

            context.builder.ins().jump(target, &[]);

            Some(Value::from_u32(0))
        },

        VirtualInstruction::StructAccess {
            struct_, field_offset, ..
        } => {
            let ptr = context.variable_table.get(struct_).unwrap().clone();
            let _type = &instruction.value.type_;

            Some(context.builder.ins().iadd_imm(ptr, *field_offset as i64))
        },

        VirtualInstruction::Store {
            memory, value, type_
        } => {
            let target = context.variable_table.get(memory).unwrap();
            let value = context.variable_table.get(value).unwrap();

            if type_.is_structure(&context.type_map) {
                let size = get_type_size(context.type_map, type_).unwrap() as u64;
                let size_literal = context.builder.ins().iconst(ir::Type::int(64).unwrap(), size as i64);

                context.builder.call_memcpy(
                    context.target_frontend_config.clone(),
                    target.clone(),
                    value.clone(),
                    size_literal
                )
            } else {
                context.builder.ins().store(MemFlags::new(), value.clone(), target.clone(), 0);
            }

            Some(
                Value::from_u32(0)
            )
        },

        VirtualInstruction::AddressOf {
            value
        } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            Some(val)
        },

        VirtualInstruction::ZExtend {
            value
        } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let _type = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(_type, context.type_map);

            Some(
                context.builder.ins().uextend(cranelift_type, val)
            )
        },

        VirtualInstruction::SExtend {
            value
        } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let _type = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(_type, context.type_map);

            Some(
                context.builder.ins().sextend(cranelift_type, val)
            )
        }

        VirtualInstruction::Trunc {
            value
        } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let _type = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(_type, context.type_map);

            Some(
                context.builder.ins().ireduce(cranelift_type, val)
            )
        }

        VirtualInstruction::NOP => Some(Value::from_u32(0)),

        VirtualInstruction::Immediate {
            value
        } => {
            let _type = &instruction.value.type_;
            let cranelift_type = get_cranelift_type(_type, context.type_map);

            Some(
                context.builder.ins().iconst(cranelift_type, *value as i64)
            )
        }

        _ => unimplemented!("Instruction not implemented: {:?}", instruction.instruction)
    }
}

fn generate_params(
    context: &mut FunctionState,
    prototype: &BytecodeFunctionPrototype,
    params: &[ValueID]
) -> Option<Vec<Value>> {
    let mut args = Vec::new();

    if is_structure(context.type_map, &prototype.return_type) {
        let temp_buffer = allocate_variable(
            context,
            get_type_size(context.type_map, &prototype.return_type)? as u32,
            None
        )?;

        args.push(temp_buffer);
    }

    for param in params.iter() {
        args.push(
            context.variable_table.get(param).cloned().unwrap()
        );
    }

    Some(args)
}