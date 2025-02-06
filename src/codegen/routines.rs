use std::clone;
use cranelift::codegen::ir;
use cranelift::codegen::ir::GlobalValue;
use cranelift::prelude::{FunctionBuilder, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Value};
use cranelift_module::{DataDescription, Module};
use crate::codegen::codegen::FunctionState;
use crate::codegen::scope::VariableTable;
use crate::lex::token::OperatorType;
use crate::parse::ast::{Expression, MemoryExpression};

pub(crate) fn allocate_variable(builder: &mut FunctionBuilder, variable_table: &mut VariableTable,
                                name: &str, type_: ir::Type, initial_value: Option<ir::Value>) -> Option<Value> {
    let stack_slot_data = StackSlotData::new(StackSlotKind::ExplicitSlot, type_.bytes(), 1);
    let stack_slot = builder.create_sized_stack_slot(stack_slot_data);
    let stack_pointer = builder.ins().stack_addr(type_, stack_slot, 0);

    if let Some(initial_value) = initial_value {
        builder.ins().stack_store(initial_value, stack_slot, 0);
    }

    variable_table.insert(
        name.to_string(),
        stack_pointer, type_
    );

    Some(stack_pointer)
}

pub(crate) fn load_value(context: &mut FunctionState, value: &Expression) -> Option<Value> {
    let (value, _) = match value {
        Expression::Memory(MemoryExpression::VariableStorage { name })
            => context.variable_table.get(name)?.clone(),

        _ => return None
    };

    Some(context.builder.ins().load(ir::types::I32, MemFlags::new(), value, 0))
}

pub(crate) fn signed_bin_op(builder: &mut FunctionBuilder, op: OperatorType, lhs: Value, rhs: Value) -> Option<Value> {
    Some(
        match op {
            OperatorType::Add => builder.ins().iadd(lhs, rhs),
            OperatorType::Subtract => builder.ins().isub(lhs, rhs),
            OperatorType::Multiply => builder.ins().imul(lhs, rhs),
            OperatorType::Divide => builder.ins().sdiv(lhs, rhs),
            OperatorType::Modulo => builder.ins().srem(lhs, rhs),
            OperatorType::Equal => builder.ins().icmp(ir::condcodes::IntCC::Equal, lhs, rhs),
            OperatorType::NotEqual => builder.ins().icmp(ir::condcodes::IntCC::NotEqual, lhs, rhs),
            OperatorType::Less => builder.ins().icmp(ir::condcodes::IntCC::SignedLessThan, lhs, rhs),
            OperatorType::LessEqual => builder.ins().icmp(ir::condcodes::IntCC::SignedLessThanOrEqual, lhs, rhs),
            OperatorType::Greater => builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThan, lhs, rhs),
            OperatorType::GreaterEqual => builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThanOrEqual, lhs, rhs),
            _ => panic!("Unimplemented operator {:?}", op)
        }
    )
}

pub(crate) fn string_literal(context: &mut FunctionState, str: &str) -> GlobalValue {
    let id = context.object_module.declare_anonymous_data(
        false,
        false
    ).unwrap();

    let mut data = DataDescription::new();
    let mut str_data = str.as_bytes().to_vec();
    str_data.push('\0' as u8);

    data.define(str_data.into_boxed_slice());

    context.object_module.define_data(id, &data).unwrap();
    context.object_module.declare_data_in_func(
        id,
        context.builder.func
    )
}
