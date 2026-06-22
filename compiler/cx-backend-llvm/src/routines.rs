use crate::GlobalState;
use crate::typing::bc_llvm_signature;
use cx_lmir::LMIRFunctionSignature;
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
        return None;
    };

    global_state
        .module
        .add_function(name, llvm_prototype, None)
        .into()
}
