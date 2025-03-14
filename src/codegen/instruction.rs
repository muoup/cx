use cranelift::codegen::gimli::ReaderOffset;
use cranelift::codegen::ir;
use cranelift::codegen::ir::stackslot::StackSize;
use cranelift::codegen::isa::TargetFrontendConfig;
use cranelift::prelude::{Block, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Value};
use cranelift_module::{DataId, Module};
use crate::codegen::FunctionState;
use crate::codegen::routines::allocate_variable;
use crate::codegen::value_type::get_cranelift_type;
use crate::lex::token::OperatorType;
use crate::parse::ast::ValueType;
use crate::parse::verify::bytecode::{BlockInstruction, VirtualInstruction};
use crate::parse::verify::verify_type::{get_intrinsic_type, get_type_size};

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

        VirtualInstruction::DirectCall {
            function, args
        } => {
            let func_id = context.function_ids.get(function).cloned().unwrap();
            let func_ref = context.object_module.declare_func_in_func(func_id, &mut context.builder.func);

            let args = args.iter()
                .map(|arg| {
                    context.variable_table.get(arg).cloned().unwrap()
                })
                .collect::<Vec<Value>>();

            let inst = context.builder.ins().call(func_ref, args.as_slice());

            context.builder.inst_results(inst)
                .first()
                .cloned()
                .or_else(|| None)
        }

        VirtualInstruction::Return { value } => {
            match value {
                Some(value) => {
                    let val = context.variable_table.get(&value).cloned().unwrap();

                    context.builder.ins().return_(&[val])
                },
                None => {
                    context.builder.ins().return_(&[])
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
                OperatorType::Add => context.builder.ins().iadd(left, right),
                OperatorType::Subtract => context.builder.ins().isub(left, right),
                OperatorType::Multiply => context.builder.ins().imul(left, right),
                OperatorType::Divide => context.builder.ins().udiv(left, right),

                OperatorType::Less => context.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThan, left, right),
                OperatorType::Greater => context.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThan, left, right),
                OperatorType::LessEqual => context.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThanOrEqual, left, right),
                OperatorType::GreaterEqual => context.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThanOrEqual, left, right),
                OperatorType::Equal => context.builder.ins().icmp(ir::condcodes::IntCC::Equal, left, right),
                OperatorType::NotEqual => context.builder.ins().icmp(ir::condcodes::IntCC::NotEqual, left, right),

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
            memory, value
        } => {
            let target = context.variable_table.get(memory).unwrap();
            let value = context.variable_table.get(value).unwrap();

            let type_ = get_intrinsic_type(context.type_map, &instruction.value.type_)?;

            if matches!(type_, &ValueType::Structured { .. }) {
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

        VirtualInstruction::NOP => Some(Value::from_u32(0)),

        _ => unimplemented!("Instruction not implemented: {:?}", instruction.instruction)
    }
}