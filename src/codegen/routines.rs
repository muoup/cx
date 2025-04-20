use std::process::id;
use crate::codegen::FunctionState;
use crate::lex::token::OperatorType;
use crate::parse::ast::ValueType;
use crate::parse::pass_verified::verify_type::get_type_size;
use cranelift::codegen::gimli::ReaderOffset;
use cranelift::codegen::ir;
use cranelift::codegen::ir::stackslot::StackSize;
use cranelift::codegen::ir::GlobalValue;
use cranelift::prelude::{FunctionBuilder, InstBuilder, StackSlotData, StackSlotKind, Value};
use cranelift_module::{DataDescription, DataId, Module};
use cranelift_object::object::SymbolScope::Linkage;
use cranelift_object::ObjectModule;

pub(crate) fn stack_alloca(context: &mut FunctionState, type_: &ValueType) -> Option<Value> {
    match type_ {
        ValueType::Structured { fields } => {
            let field_values = fields.iter()
                .map(|init| stack_alloca(context, &init.type_))
                .collect::<Vec<_>>();

            Some(field_values[0]?.to_owned())
        },

        _ => allocate_variable(
            context,
            get_type_size(&context.type_map, type_)? as u32,
            None
        )
    }
}

pub(crate) fn allocate_variable(context: &mut FunctionState, bytes: u32, initial_value: Option<ir::Value>) -> Option<Value> {
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

pub(crate) fn string_literal(object_module: &mut ObjectModule, str: &str) -> DataId {
    let id = object_module.declare_anonymous_data(
        false,
        false
    ).unwrap();

    let mut str_data = str.to_owned().into_bytes();
    str_data.push('\0' as u8);

    let mut data = DataDescription::new();
    data.define(str_data.into_boxed_slice());

    object_module.define_data(id, &data).unwrap();
    object_module.declare_data_in_data(id, &mut data);

    id
}