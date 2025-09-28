use inkwell::values::FunctionValue;
use cx_data_mir::MIRFunctionPrototype;
use cx_util::log_error;
use crate::GlobalState;
use crate::typing::bc_llvm_prototype;

pub(crate) fn get_function<'a>(global_state: &GlobalState<'a>, prototype: &MIRFunctionPrototype) -> Option<FunctionValue<'a>> {
    if let Some(function_val) = global_state
        .module
        .get_function(&prototype.name) {
        return Some(function_val);
    };

    let Some(llvm_prototype) = bc_llvm_prototype(global_state, prototype) else {
        log_error!("Failed to get LLVM prototype for function: {}", &prototype.name);
    };

    global_state.module.add_function(
        &prototype.name, llvm_prototype, None,
    ).into()
}