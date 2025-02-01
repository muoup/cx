use cranelift::codegen::ir;
use cranelift::prelude::{FunctionBuilder, InstBuilder, StackSlotData, StackSlotKind, Value};
use crate::codegen::scope::VariableTable;

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