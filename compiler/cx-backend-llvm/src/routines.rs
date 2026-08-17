use crate::GlobalState;
use crate::error::LLVMResult;
use crate::typing::{apply_llvm_parameter_attributes, bc_llvm_signature};
use cx_lmir::LMIRFunctionSignature;
use inkwell::values::FunctionValue;

pub(crate) fn get_function<'a>(
    global_state: &GlobalState<'a>,
    name: &str,
    signature: &LMIRFunctionSignature,
) -> LLVMResult<FunctionValue<'a>> {
    if let Some(function_val) = global_state.module.get_function(name) {
        return Ok(function_val);
    }

    let llvm_prototype = bc_llvm_signature(global_state, signature)?;

    let function = global_state.module.add_function(name, llvm_prototype, None);
    apply_llvm_parameter_attributes(
        global_state.context,
        global_state.architecture,
        &function,
        signature,
    )?;
    Ok(function)
}
