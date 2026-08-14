use super::returns::{apply_call_abi_attributes, codegen_call_return};
use super::support::inst_num;
use crate::routines::get_function;
use crate::typing::{any_to_basic_val, bc_llvm_signature};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::{LMIRFunctionSignature, LMIRValue};
use cx_util::identifier::CXIdent;

pub(super) fn generate_direct_call<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    func: &CXIdent,
    args: &[LMIRValue],
    method_sig: &LMIRFunctionSignature,
) -> Option<CodegenValue<'a>> {
    let function_val = get_function(global_state, func.as_str(), method_sig)?;
    let arg_vals = args
        .iter()
        .map(|arg| {
            let val = function_state.get_value(arg)?.get_value();
            let basic_val = any_to_basic_val(val)?;
            Some(basic_val.into())
        })
        .collect::<Option<Vec<_>>>()?;

    let call = function_state
        .builder
        .build_direct_call(function_val, arg_vals.as_slice(), inst_num().as_str())
        .unwrap();
    apply_call_abi_attributes(global_state, &call, method_sig);

    codegen_call_return(function_state, method_sig, &call)
}

pub(super) fn generate_indirect_call<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    func_ptr: &LMIRValue,
    args: &[LMIRValue],
    method_sig: &LMIRFunctionSignature,
) -> Option<CodegenValue<'a>> {
    let ptr = function_state.get_value(func_ptr)?.get_value();
    let fn_type = bc_llvm_signature(global_state, method_sig)?;
    let arg_vals = args
        .iter()
        .map(|arg| {
            let val = function_state.get_value(arg)?.get_value();
            let basic_val = any_to_basic_val(val)?;
            Some(basic_val.into())
        })
        .collect::<Option<Vec<_>>>()?;

    let call = function_state
        .builder
        .build_indirect_call(
            fn_type,
            ptr.into_pointer_value(),
            arg_vals.as_slice(),
            inst_num().as_str(),
        )
        .unwrap();
    apply_call_abi_attributes(global_state, &call, method_sig);

    codegen_call_return(function_state, method_sig, &call)
}
