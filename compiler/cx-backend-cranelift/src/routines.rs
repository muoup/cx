use crate::inst_calling::prepare_function_sig;
use crate::FunctionState;
use cranelift_module::{FuncId, Linkage, Module};
use cx_lmir::{LMIRFunctionSignature, LinkageType};
use cx_util::CXResult;

pub fn get_function(
    context: &mut FunctionState,
    name: &str,
    signature: &LMIRFunctionSignature,
) -> CXResult<FuncId> {
    if let Some(func_id) = context.function_ids.get(name) {
        return Ok(*func_id);
    }

    let signature = prepare_function_sig(context.object_module, signature)?;
    let linkage = convert_linkage(LinkageType::External);

    let func_id = context
        .object_module
        .declare_function(name, linkage, &signature)
        .unwrap();

    context.function_ids.insert(name.to_string(), func_id);

    Ok(func_id)
}

pub fn convert_linkage(linkage: LinkageType) -> cranelift_module::Linkage {
    match linkage {
        LinkageType::ODR => Linkage::Local,
        LinkageType::Static => Linkage::Local,
        LinkageType::Standard => Linkage::Export,
        LinkageType::External => Linkage::Import,
    }
}
