use crate::{CodegenValue, GlobalState};
use inkwell::values::AnyValueEnum;

pub(super) fn generate_get_function_addr<'a>(
    global_state: &GlobalState<'a>,
    func: &str,
) -> Option<CodegenValue<'a>> {
    let function_val = global_state
        .module
        .get_function(func)
        .unwrap()
        .as_global_value()
        .as_pointer_value();

    // Calling as_any_value_enum() on a function pointer produces a
    // FunctionValue instead of the pointer value LLVM expects here.
    let any_value_enum = AnyValueEnum::PointerValue(function_val);

    Some(CodegenValue::Value(any_value_enum))
}
