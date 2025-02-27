use cranelift::codegen::gimli::ReaderOffset;
use cranelift::codegen::ir;
use cranelift::codegen::ir::stackslot::StackSize;
use cranelift::prelude::{Block, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Value};
use cranelift_module::{DataId, Module};
use crate::codegen::FunctionState;
use crate::codegen::routines::allocate_variable;
use crate::codegen::value_type::get_cranelift_type;
use crate::lex::token::OperatorType;
use crate::parse::verify::bytecode::{BlockInstruction, VirtualInstruction};

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
            let pointer_type = context.pointer_type;
            let global_val = context.global_strs.get(*str_id as usize).cloned().unwrap();

            context.object_module.declare_data_in_func(
                DataId::from_u32(global_val.as_u32()),
                &mut context.builder.func
            );

            Some(context.builder.ins().global_value(pointer_type, global_val))
        },

        VirtualInstruction::DirectCall {
            function, args
        } => {
            let func_id = context.function_ids.get(function).cloned().unwrap();
            let func_ref = context.object_module.declare_func_in_func(func_id, &mut context.builder.func);

            let args = args.iter()
                .map(|arg| {
                    context.variable_table.get(&arg).cloned().unwrap()
                })
                .collect::<Vec<Value>>();

            let inst = context.builder.ins().call(func_ref, args.as_slice());

            context.builder.inst_results(inst)
                .first()
                .cloned()
                .or_else(|| Some(Value::from_u32(0)))
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
            Some(Value::from_u32(0))
        },

        VirtualInstruction::FunctionParameter { type_, name, param_index } => {
            let value = context.fn_params.get(*param_index as usize).cloned().unwrap();
            let crane_type = get_cranelift_type(type_, context.type_map);

            allocate_variable(
                &mut context.builder, &mut context.variable_table,
                name.as_str(), crane_type,
                Some(value)
            )
        },

        VirtualInstruction::Load { value, type_ } => {
            let val = context.variable_table.get(value).cloned().unwrap();
            let cranelift_type = get_cranelift_type(type_, context.type_map);

            Some(
                context.builder.ins()
                    .load(
                        cranelift_type,
                        ir::MemFlags::new(),
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
            let target = Block::from_u32(*target as u32);

            context.builder.ins().jump(target, &[]);

            Some(Value::from_u32(0))
        },

        VirtualInstruction::StructAccess {
            struct_, type_, field_offset, field_index
        } => {
            let ptr = context.variable_table.get(struct_)?.clone();

            Some(context.builder.ins().iadd_imm(ptr, *field_offset as i64))
        },

        VirtualInstruction::Assign {
            target, value
        } => {
            let target = context.variable_table.get(target)?;
            let value = context.variable_table.get(value)?;

            context.builder.ins().store(MemFlags::new(), value.clone(), target.clone(), 0);

            Some(
                Value::from_u32(0)
            )
        }

        _ => unimplemented!("Instruction not implemented: {:?}", instruction.instruction)
    }
}