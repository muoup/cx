use cranelift::codegen::gimli::ReaderOffset;
use cranelift::codegen::ir;
use cranelift::codegen::ir::stackslot::StackSize;
use cranelift::prelude::{FunctionBuilder, InstBuilder, StackSlotData, StackSlotKind, Value};
use cranelift_module::{DataDescription, DataId, Module};
use cranelift_object::ObjectModule;
use cx_data_ast::lex::token::OperatorType;
use crate::FunctionState;

pub(crate) fn allocate_variable(context: &mut FunctionState, bytes: u32, initial_value: Option<Value>) -> Option<Value> {
    let stack_slot_data = StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        StackSize::from_u32(bytes),
        1
    );
    let stack_slot = context.builder.create_sized_stack_slot(stack_slot_data);
    let stack_pointer = context.builder.ins().stack_addr(context.pointer_type, stack_slot, 0);

    if let Some(initial_value) = initial_value {
        context.builder.ins().stack_store(initial_value, stack_slot, 0);
    }

    Some(stack_pointer)
}

pub(crate) fn signed_bin_op(builder: &mut FunctionBuilder, op: OperatorType, lhs: Value, rhs: Value) -> Option<Value> {
    Some(
        match op {
            OperatorType::Plus => builder.ins().iadd(lhs, rhs),
            OperatorType::Minus => builder.ins().isub(lhs, rhs),
            OperatorType::Asterisk => builder.ins().imul(lhs, rhs),
            OperatorType::Slash => builder.ins().sdiv(lhs, rhs),
            OperatorType::Percent => builder.ins().srem(lhs, rhs),
            OperatorType::Equal => builder.ins().icmp(ir::condcodes::IntCC::Equal, lhs, rhs),
            OperatorType::NotEqual => builder.ins().icmp(ir::condcodes::IntCC::NotEqual, lhs, rhs),
            OperatorType::Less => builder.ins().icmp(ir::condcodes::IntCC::SignedLessThan, lhs, rhs),
            OperatorType::LessEqual => builder.ins().icmp(ir::condcodes::IntCC::SignedLessThanOrEqual, lhs, rhs),
            OperatorType::Greater => builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThan, lhs, rhs),
            OperatorType::GreaterEqual => builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThanOrEqual, lhs, rhs),
            _ => panic!("Unimplemented operator {op:?}")
        }
    )
}

pub(crate) fn string_literal(object_module: &mut ObjectModule, str: &str) -> DataId {
    let id = object_module.declare_anonymous_data(
        false,
        false
    ).unwrap();

    let mut str_data = str.to_owned().into_bytes();
    str_data.push(b'\0');

    let mut data = DataDescription::new();
    data.define(str_data.into_boxed_slice());

    object_module.define_data(id, &data).unwrap();
    object_module.declare_data_in_data(id, &mut data);

    id
}