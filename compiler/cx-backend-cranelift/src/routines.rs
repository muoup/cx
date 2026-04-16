use crate::inst_calling::prepare_function_sig;
use crate::FunctionState;
use cranelift_module::{FuncId, Linkage, Module};
use cx_lmir::{LMIRFunctionSignature, LinkageType};

pub fn get_function(
    context: &mut FunctionState,
    name: &str,
    signature: &LMIRFunctionSignature,
) -> Option<FuncId> {
    if let Some(func_id) = context.function_ids.get(name) {
        return Some(*func_id);
    }

    let Some(signature) = prepare_function_sig(context.object_module, signature) else {
        panic!(
            "Failed to prepare function signature for function: {:?}",
            name
        );
    };
    let linkage = convert_linkage(LinkageType::External);

    let func_id = context
        .object_module
        .declare_function(name, linkage, &signature)
        .unwrap();

    context.function_ids.insert(name.to_string(), func_id);

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
