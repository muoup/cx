use super::returns::build_direct_return_from_memory;
use crate::typing::any_to_basic_val;
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::{LMIRReturnABI, LMIRValue};

pub(super) fn generate_return<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: Option<&LMIRValue>,
) -> Option<CodegenValue<'a>> {
    let Some(value) = value else {
        function_state.builder.build_return(None).unwrap();
        return Some(CodegenValue::Null);
    };

    let value = function_state.get_value(value)?;
    if function_state.signature.return_type.is_memory_resident()
        && matches!(
            function_state.signature.return_abi,
            LMIRReturnABI::Direct { .. }
        )
    {
        let return_value =
            build_direct_return_from_memory(global_state, function_state, value.get_value())?;
        function_state
            .builder
            .build_return(Some(&return_value))
            .unwrap();
        return Some(CodegenValue::Null);
    }

    let basic_value = any_to_basic_val(value.get_value())?;
    function_state
        .builder
        .build_return(Some(&basic_value))
        .unwrap();
    Some(CodegenValue::Null)
}

pub(super) fn generate_unreachable<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
) -> Option<CodegenValue<'a>> {
    function_state.builder.build_unreachable().ok()?;
    Some(CodegenValue::Null)
}
