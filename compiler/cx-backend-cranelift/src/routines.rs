use crate::FunctionState;
use cranelift::codegen::gimli::ReaderOffset;
use cranelift::codegen::ir::stackslot::StackSize;
use cranelift::prelude::{InstBuilder, StackSlotData, StackSlotKind, Value};
use cranelift_module::{DataDescription, DataId, FuncId, Module};
use cranelift_object::ObjectModule;
use cx_data_bytecode::BCFunctionPrototype;
use crate::inst_calling::prepare_function_sig;

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

pub fn get_function(context: &mut FunctionState, prototype: &BCFunctionPrototype) -> Option<FuncId> {
    if let Some(func_id) = context.function_ids.get(&prototype.name) {
        return Some(*func_id);
    }
    
    let Some(signature) = prepare_function_sig(&mut context.object_module, prototype) else {
        panic!("Failed to prepare function signature for function: {:?}", prototype.name);
    };

    let linkage = match prototype.linkage {
        cx_data_bytecode::LinkageType::ODR => cranelift_module::Linkage::Local,
        cx_data_bytecode::LinkageType::Static => cranelift_module::Linkage::Local,
        cx_data_bytecode::LinkageType::Public => cranelift_module::Linkage::Export,
        cx_data_bytecode::LinkageType::Private => cranelift_module::Linkage::Local,
    };
    
    let func_id = context.object_module
        .declare_function(prototype.name.as_str(), linkage, &signature)
        .unwrap();

    context.function_ids.insert(prototype.name.clone(), func_id);

    Some(func_id)
}