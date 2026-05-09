use crate::value_type::get_cranelift_abi_type;
use crate::{CodegenValue, FunctionState};
use cranelift::codegen::ir;
use cranelift::codegen::ir::{FuncRef, Inst};
use cranelift::prelude::{Signature, Value};
use cranelift_module::{FuncId, Module};
use cranelift_object::ObjectModule;
use cx_lmir::{LMIRFunctionSignature, LMIRParameter, LMIRValue};
use cx_util::CXResult;

pub(crate) fn prepare_function_sig(
    object_module: &mut ObjectModule,
    signature: &LMIRFunctionSignature,
) -> CXResult<Signature> {
    let mut sig = Signature::new(object_module.target_config().default_call_conv);

    if !signature.return_type.is_void() {
        sig.returns
            .push(get_cranelift_abi_type(&signature.return_type)?);
    }

    for LMIRParameter { _type, .. } in signature.params.iter() {
        sig.params.push(get_cranelift_abi_type(_type)?);
    }

    Ok(sig)
}

pub(crate) fn prepare_method_call<'a>(
    context: &'a mut FunctionState,
    func: &LMIRValue,
    args: &'a [LMIRValue],
) -> CXResult<(CodegenValue, Vec<Value>)> {
    let val = context.get_value(func)?;
    let params = prepare_parameters(context, args)?;

    Ok((val, params))
}

pub(crate) fn prepare_parameters<'a>(
    context: &'a mut FunctionState,
    args: &'a [LMIRValue],
) -> CXResult<Vec<Value>> {
    args.iter()
        .map(|arg| context.get_value(arg).map(|cg| CodegenValue::as_value(&cg)))
        .collect::<CXResult<Vec<_>>>()
}

pub(crate) fn get_method_return(context: &FunctionState, inst: Inst) -> Option<CodegenValue> {
    context
        .builder
        .inst_results(inst)
        .first()
        .cloned()
        .map(CodegenValue::Value)
}

pub(crate) fn get_func_ref(
    context: &mut FunctionState,
    func_id: FuncId,
    signature: &LMIRFunctionSignature,
    args: &[Value],
) -> CXResult<FuncRef> {
    if !signature.var_args || args.len() == signature.params.len() {
        return Ok(context
            .object_module
            .declare_func_in_func(func_id, context.builder.func));
    }

    let mut sig = prepare_function_sig(context.object_module, signature)?;

    for i in signature.params.len()..args.len() {
        let arg_type = context.builder.func.dfg.value_type(args[i]);

        sig.params.push(ir::AbiParam::new(arg_type));
    }

    // S/O the rustc codegen team for this, see: https://github.com/rust-lang/rustc_codegen_cranelift/blob/master/src/abi/mod.rs
    let func_ref = context
        .object_module
        .declare_func_in_func(func_id, context.builder.func);

    let sig_ref = context
        .builder
        .func
        .dfg
        .ext_funcs
        .get(func_ref)
        .unwrap()
        .signature;

    context.builder.func.dfg.signatures[sig_ref] = sig;

    Ok(func_ref)
}
