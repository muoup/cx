use crate::GlobalState;
use crate::typing::bc_llvm_signature;
use cx_lmir::LMIRFunctionSignature;
use cx_util::log_error;
use inkwell::values::FunctionValue;

pub(crate) fn get_function<'a>(
    global_state: &GlobalState<'a>,
    name: &str,
    signature: &LMIRFunctionSignature,
) -> Option<FunctionValue<'a>> {
    if let Some(function_val) = global_state.module.get_function(name) {
        return Some(function_val);
    };

    let Some(llvm_prototype) = bc_llvm_signature(global_state, signature) else {
        log_error!("Failed to get LLVM prototype for function: {}", name);
    };

    global_state
        .module
        .add_function(name, llvm_prototype, None)
        .into()
}
