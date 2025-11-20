use crate::inst_calling::prepare_function_sig;
use crate::FunctionState;
use cranelift_module::{FuncId, Linkage, Module};
use cx_bytecode_data::{LinkageType, MIRFunctionPrototype};

pub fn get_function(
    context: &mut FunctionState,
    prototype: &MIRFunctionPrototype,
) -> Option<FuncId> {
    if let Some(func_id) = context.function_ids.get(&prototype.name) {
        return Some(*func_id);
    }

    let Some(signature) = prepare_function_sig(context.object_module, prototype) else {
        panic!(
            "Failed to prepare function signature for function: {:?}",
            prototype.name
        );
    };
    let linkage = convert_linkage(prototype.linkage);

    let func_id = context
        .object_module
        .declare_function(prototype.name.as_str(), linkage, &signature)
        .unwrap();

    context.function_ids.insert(prototype.name.clone(), func_id);

    Some(func_id)
}

pub fn convert_linkage(linkage: LinkageType) -> cranelift_module::Linkage {
    match linkage {
        LinkageType::ODR => Linkage::Local,
        LinkageType::Static => Linkage::Local,
        LinkageType::Standard => Linkage::Export,
        LinkageType::External => Linkage::Import,
    }
}
